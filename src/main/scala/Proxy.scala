package org.example.socksproxy

import java.net.{Inet4Address, Inet6Address}

sealed trait Proxy
case class SocksV4(command: Socks4Command, port: Int, address: Inet4Address, clientId: String) extends Proxy
case class SocksV4A(command: Socks4Command, port: Int, clientId: String, domain: String) extends Proxy
case class SocksV5(auth: Socks5Authorization, header: Option[Socks5Header]) extends Proxy

object HttpTunnel extends Proxy
case class Http(address: String) extends Proxy



sealed trait Socks4Command
object Socks4Command {
  object Connect extends Socks4Command
  object UdpAssociation extends Socks4Command
}

sealed trait Socks5Command
object Socks5Command {
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