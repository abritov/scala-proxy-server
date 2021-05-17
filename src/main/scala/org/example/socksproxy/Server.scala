package org.example.socksproxy

import cats.effect.{Async, Concurrent, Ref, Sync}
import cats.implicits._
import cats.{FlatMap, MonadError}
import com.comcast.ip4s.{Ipv4Address, Port}
import fs2.Stream
import fs2.io.net.{Network, Socket}

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

  private def handleClient[F[_]: Concurrent: Console](
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

  private def processIncoming[F[_]: Console](
                                     clients: Clients[F],
                                     clientId: UUID,
                                     messageSocket: MessageSocket[F, Proxy, ProxyResponse]
                                   )(implicit F: MonadError[F, Throwable]): Stream[F, Nothing] =
    messageSocket.read.evalMap {
      case Proxy.SocksV4(command, port, address, clientId, None) => {
        Console[F].println(s"socks proxy v4 $command $port $address")
      }
      case Proxy.SocksV4(command, port, _, clientId, Some(domain)) => {
        Console[F].println(s"socks proxy v4a $command $port $domain")
      }
      case Proxy.SocksV5Authorization(count, protocols) => {
        Console[F].println("socks proxy v5")
      }
    }.drain
}
