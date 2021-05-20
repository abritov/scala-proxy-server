package org.example.socksproxy

import cats.effect.{Async, Concurrent, MonadCancel, Ref, Sync}
import cats.implicits._
import cats.{FlatMap, MonadError}
import com.comcast.ip4s.{Host, Ipv4Address, Port, SocketAddress}
import fs2.Stream
import fs2.io.net.{Network, Socket}
import scodec.stream.{StreamDecoder, StreamEncoder}

import java.util.UUID

object Server {
  private case class ConnectedClient[F[_]](id: UUID,
                                           username: Option[String],
                                           messageSocket: MessageSocket[F, Proxy, ProxyResponse])

  private object ConnectedClient {
    def apply[F[_] : Async](socket: Socket[F]): F[ConnectedClient[F]] =
      for {
        id <- Sync[F].delay(UUID.randomUUID)
        messageSocket <- MessageSocket(
          socket,
          Proxy.codec,
          ProxyResponse.codec,
          128
        )
      } yield ConnectedClient(id, None, messageSocket)
  }

  private class Clients[F[_]: Concurrent](ref: Ref[F, Map[UUID, ConnectedClient[F]]]) {
    def get(id: UUID): F[Option[ConnectedClient[F]]] = ref.get.map(_.get(id))
    def all: F[List[ConnectedClient[F]]] = ref.get.map(_.values.toList)
    def named: F[List[ConnectedClient[F]]] =
      ref.get.map(_.values.toList.filter(_.username.isDefined))
    def register(state: ConnectedClient[F]): F[Unit] =
      ref.update(oldClients => oldClients + (state.id -> state))
    def unregister(id: UUID): F[Option[ConnectedClient[F]]] =
      ref.modify(old => (old - id, old.get(id)))
  }

  private object Clients {
    def apply[F[_]: Concurrent]: F[Clients[F]] =
      Ref[F]
        .of(Map.empty[UUID, ConnectedClient[F]])
        .map(ref => new Clients(ref))
  }

  def start[F[_]: Async: Network: Console](port: Port) =
    Stream.exec(Console[F].info(s"Starting server on port $port")) ++
      Stream
        .eval(Clients[F])
        .flatMap { clients =>
          Network[F].server(port = Some(port)).map { clientSocket =>
            def unregisterClient(state: ConnectedClient[F]) =
              clients.unregister(state.id) *> Console[F].info(s"Unregistered client ${state.id}")
            Stream
              .bracket(ConnectedClient[F](clientSocket).flatTap(clients.register))(
                unregisterClient
              )
              .flatMap(client => handleClient[F](clients, client, clientSocket))
              .scope
          }
        }
        .parJoinUnbounded

  private def handleClient[F[_]: Concurrent: Console: Network](
                                                       clients: Clients[F],
                                                       clientState: ConnectedClient[F],
                                                       clientSocket: Socket[F]
                                                     ): Stream[F, Nothing] = {
    logNewClient(clientState, clientSocket) ++
      processIncoming(clients, clientState.id, clientState.messageSocket)
  }.handleErrorWith {
    case _: UserQuit =>
      Stream.exec(Console[F].info(s"Client quit ${clientState.id}"))
    case err =>
      Stream.exec(
        Console[F].errorln(s"Fatal error for client ${clientState.id} - $err")
      )
  }

  private def logNewClient[F[_]: FlatMap: Console](
                                                    clientState: ConnectedClient[F],
                                                    clientSocket: Socket[F]
                                                  ): Stream[F, Nothing] =
    Stream.exec(clientSocket.remoteAddress.flatMap { clientAddress =>
      Console[F].info(s"Accepted client ${clientState.id} on $clientAddress")
    })

  private def processIncoming[F[_]: Console: Network: Concurrent](
                                     clients: Clients[F],
                                     clientId: UUID,
                                     messageSocket: MessageSocket[F, Proxy, ProxyResponse]
                                   )(implicit F: MonadCancel[F, Throwable]): Stream[F, Nothing] =
    messageSocket.read.flatMap {
      case Proxy.SocksV4(command, port, address, clientId, None) =>
        Stream.exec(Console[F].println(s"socks proxy v4 $command $port $address")) ++
        Stream
          .resource(Network[F].client(SocketAddress(address, Port(port).get)))
          .flatMap { socket =>
            val remoteToClient = socket
              .reads
              .through(messageSocket.writeBytes)
            Stream.exec(messageSocket.write1(ProxyResponse.Socks4Response.requestGranted)) ++
              Stream.exec(Console[F].println(s"v4 requestGranted sent")) ++
                messageSocket
                  .readBytes
                  .through(socket.writes)
                  .concurrently(remoteToClient)
          }
          .handleErrorWith { error =>
            Stream.exec(messageSocket.write1(ProxyResponse.Socks4Response.requestDenied))
          }

      case Proxy.SocksV4(command, port, _, clientId, Some(domain)) =>
        Stream.exec(Console[F].println(s"socks proxy v4a $command $port $domain")) ++
          Stream
            .resource(Network[F].client(SocketAddress.fromStringHostname(s"$domain:$port").get))
            .flatMap { socket =>
              val remoteToClient = socket
                .reads
                .through(messageSocket.writeBytes)
              Stream.exec(messageSocket.write1(ProxyResponse.Socks4Response.requestGranted)) ++
                messageSocket
                  .readBytes
                  .through(socket.writes)
                  .concurrently(remoteToClient)
            }
            .handleErrorWith { error =>
              Stream.exec(messageSocket.write1 (ProxyResponse.Socks4Response.requestDenied))
            }

      case Proxy.SocksV5(Some(SocksV5Authorization(count, protocols)), None) =>
        Stream.exec(Console[F].println("socks proxy v5")) ++
          Stream.exec(messageSocket.write1(ProxyResponse.Socks5AuthMethodAccepted(protocols.head))) ++
          Stream.exec(Console[F].println(s"v5 client accepted")) ++
        messageSocket.read.flatMap {
          case Proxy.SocksV5(None, Some(Socks5Header(command, addressType, port))) => {
            val socketAddress: SocketAddress[Host] = addressType match {
              case Socks5Address.IpV4(address) => SocketAddress(address, Port(port).get)
              case Socks5Address.Domain(domain) => SocketAddress.fromStringHostname(s"$domain:$port").get
            }
            import ProxyResponse.Socks5Response
            command match {
              case Socks5Command.Connect => Stream
                  .resource(Network[F].client(socketAddress))
                  .flatMap { socket =>
                    val remoteToClient = socket
                      .reads
                      .through(messageSocket.writeBytes)
                    Stream.exec(Console[F].println(s"v5 remote connection ok $socketAddress")) ++
                    Stream.exec(
                      messageSocket.write1(Socks5Response.requestGranted(addressType))
                    ) ++
                      messageSocket
                        .readBytes
                        .through(socket.writes)
                        .concurrently(remoteToClient)
                  }
                  .handleErrorWith { error =>
                    Stream.eval(messageSocket.write1(Socks5Response.endpointUnavailable(addressType)))
                  }
              case Socks5Command.PortBinding => Stream.exec(
                messageSocket.write1(Socks5Response.notSupported(addressType)))
              case Socks5Command.UdpAssociation => Stream.exec(
                messageSocket.write1(Socks5Response.notSupported(addressType)))
            }
          }
        }
    }.drain
}
