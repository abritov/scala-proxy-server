package org.example.socksproxy

import com.comcast.ip4s.{Ipv4Address, Ipv6Address}
import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._


sealed trait Proxy
object Proxy {
  implicit val ipv4Codec: Codec[Ipv4Address] = uint32 xmap (Ipv4Address.fromLong, _.toLong)

  case class SocksV4(command: Socks4Command, port: Int, address: Ipv4Address, clientId: String, domain: Option[String]) extends Proxy
  object SocksV4 {
    implicit val codec: Codec[SocksV4] = {
        constant((hex"04")) ::
        ("command" | Codec[Socks4Command]) ::
          ("port" | uint16) ::
          (("address" | ipv4Codec) flatPrepend { fields =>
            ("clientId" | variableSizeBytes(uint8, ascii)) ::
            ("domain" | conditional(fields.toBytes match {
                case Array(0, 0, 0, _) => true
                case _ => false
              }, variableSizeBytes(uint8, ascii)).hlist)
          })
    }.as[SocksV4]
  }

  case class SocksV5(auth: Socks5Authorization, header: Socks5Header) extends Proxy
  object SocksV5 {
    implicit val codec: Codec[SocksV5] = {
      constant((hex"05")) ::
      ("auth" | Codec[Socks5Authorization]) ::
        ("header" | Codec[Socks5Header])
    }.as[SocksV5]
  }

  object HttpTunnel extends Proxy

  case class Http(address: String) extends Proxy
}



sealed trait Socks4Command
object Socks4Command {
  implicit val codec: DiscriminatorCodec[Socks4Command, Int] = mappedEnum(
    uint8,
    Socks4Command.Connect -> 1,
    Socks4Command.PortBinding -> 2
  )

  object Connect extends Socks4Command
  object PortBinding extends Socks4Command
}

sealed trait Socks5Command
object Socks5Command {
  implicit val codec: DiscriminatorCodec[Socks5Command, Int] = mappedEnum(
    uint8,
    Socks5Command.Connect -> 1,
    Socks5Command.PortBinding -> 2,
    Socks5Command.UdpAssociation -> 3
  )
  object Connect extends Socks5Command
  object PortBinding extends Socks5Command
  object UdpAssociation extends Socks5Command
}

case class Socks5Authorization(supportedAuthProtocolsCount: Int, authProtocols: Vector[Int])
object Socks5Authorization {
  implicit val codec: Codec[Socks5Authorization] = {
    (("supportedAuthProtocolsCount" | uint8) flatPrepend { count =>
      ("authProtocols" | vectorOfN(provide(count), uint8)).hlist
    })
  }.as[Socks5Authorization]
}

sealed trait Socks5Address
object Socks5Address {
  implicit val codec: Codec[Socks5Address] = discriminated[Socks5Address]
    .by(uint8)
    .typecase(1, Proxy.ipv4Codec.xmap[IpV4](Socks5Address.IpV4, _.address))
    .typecase(3, variableSizeBytes(uint8, ascii).xmap[Domain](Socks5Address.Domain, _.domain))
  case class IpV4(address: Ipv4Address) extends Socks5Address
  case class Domain(domain: String) extends Socks5Address
}

case class Socks5Header(command: Socks5Command, address: Socks5Address, port: Int)
object Socks5Header {
  implicit val codec: Codec[Socks5Header] = {
    ("command" | Codec[Socks5Command]) ::
      constant(hex"00") ::
      ("address" | Codec[Socks5Address]) ::
      ("port" | uint16)
  }.as[Socks5Header]
}