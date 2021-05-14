package org.example.socksproxy

import cats.effect.{Async, Concurrent, Ref, Sync}
import cats.implicits._
import cats.{FlatMap, MonadError}
import com.comcast.ip4s.Port
import fs2.Stream
import fs2.io.net.{Network, Socket}

import java.util.UUID

object Server {
  private case class ConnectedClient[F[_]](id: UUID,
                                           username: Option[String],
                                           messageSocket: MessageSocket[F, ProxyResponse, Proxy])

  private object ConnectedClient {
    def apply[F[_] : Async](socket: Socket[F]): F[ConnectedClient[F]] =
      for {
        id <- Sync[F].delay(UUID.randomUUID)
        messageSocket <- MessageSocket(
          socket,
          ProxyResponse.codec,
          Proxy.codec,
          1024
        )
      } yield ConnectedClient(id, None, messageSocket)
  }
}
