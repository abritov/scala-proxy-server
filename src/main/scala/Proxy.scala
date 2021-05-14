package org.example.socksproxy

import com.comcast.ip4s.{Ipv4Address, Ipv6Address}
import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

object Address {
  implicit val codec = {
    (("address_type" | uint8) >>:~ { addressType =>
      ("hardware_address" | conditional(
        addressType == 0, uint32)) ::
        ("network_address" | conditional(
          addressType == 1, variableSizeBytes(uint8, ascii)))
    })
  }
}

sealed trait Proxy
object Proxy {
  implicit val ipv4Codec: Codec[Ipv4Address] = uint32 xmap (Ipv4Address.fromLong, _.toLong)

  private object SocksV4Type extends Enumeration {
    type SocksV4Type = Value
    val SOCKS_4: Proxy.SocksV4Type.Value = Value(0)
    val SOCKS_4A: Proxy.SocksV4Type.Value = Value(1)
  }
  import SocksV4Type._

  private class SocksV4AddressTypeCodec extends Codec[SocksV4Type] {
    private val codec: Codec[(Int ~ Socks4Command ~ Int ~ Int ~ Int ~ Int ~ Int)] = uint8 ~ Codec[Socks4Command] ~ uint16 ~ uint8 ~ uint8 ~ uint8 ~ uint8

    override def encode(value: SocksV4Type): Attempt[BitVector] = Attempt.successful(BitVector.empty)

    override def sizeBound: SizeBound = SizeBound.exact(8 * 8)

    override def decode(bits: BitVector): Attempt[DecodeResult[SocksV4Type]] =
      codec
        .decode(bits)
        .map({
          case DecodeResult(((((((_, _), _), a), b), c), d), buffer) => (a, b, c, d) match {
              case (0, 0, 0, _) => DecodeResult(SOCKS_4A, buffer)
              case _ => DecodeResult(SOCKS_4, buffer)
          }})

    override def toString = "SocksV4AddressTypeCodec"
  }

  case class SocksV4(command: Socks4Command, port: Int, address: Ipv4Address, clientId: String) extends Proxy
  object SocksV4 {
    implicit val codec: Codec[SocksV4] = {
        constant((hex"04")) ::
        ("command" | Codec[Socks4Command]) ::
          ("port" | uint16) ::
          ("address" | ipv4Codec) ::
          ("clientId" | variableSizeBytes(uint8, ascii))
    }.as[SocksV4]
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
  case class IpV4(address: Ipv4Address) extends Socks5Address
  case class Domain(domain: String) extends Socks5Address
  case class IpV6(address: Ipv6Address) extends Socks5Address
}

class Socks5Header(command: Socks5Command, address: Socks5Address)