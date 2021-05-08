package org.example.socksproxy

import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

import java.net.{Inet4Address, Inet6Address}

sealed trait Proxy
object Proxy {
  case class SocksV4(command: Socks4Command, port: Int, address: Inet4Address, clientId: String) extends Proxy
  object SocksV4 {
  }

  case class SocksV4A(command: Socks4Command, port: Int, clientId: String, domain: String) extends Proxy

  case class SocksV5(auth: Socks5Authorization, header: Option[Socks5Header]) extends Proxy

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

class Socks5Authorization(supportedAuthProtocolsCount: Int, authProtocols: List[Byte])

sealed trait Socks5Address
object Socks5Address {
  case class IpV4(address: Inet4Address) extends Socks5Address
  case class Domain(domain: String) extends Socks5Address
  case class IpV6(address: Inet6Address) extends Socks5Address
}

class Socks5Header(command: Socks5Command, address: Socks5Address)