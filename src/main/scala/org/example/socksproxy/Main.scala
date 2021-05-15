package org.example.socksproxy

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxOption
import com.comcast.ip4s.{Ipv4Address, Port}
import com.monovore.decline.{Command, Opts}
import scodec.Codec
import scodec.bits.HexStringSyntax

object Main extends IOApp {
  private val argsParser: Command[Port] =
    Command("scala-proxy-server", "Scala proxy Chat Server") {
      Opts
        .option[Int]("port", "Port to bind for connection requests")
        .withDefault(5555)
        .mapValidated(p => Port(p).toValidNel("Invalid port number"))
    }

  def run(args: List[String]): IO[ExitCode] =
    argsParser.parse(args) match {
      case Left(help) => IO(System.err.println(help)).as(ExitCode.Error)
      case Right(port) =>
        Console
          .create[IO]
          .flatMap(implicit console => Server.start[IO](port).compile.drain)
          .as(ExitCode.Success)
    }
}
