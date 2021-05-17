package org.example.socksproxy

import com.comcast.ip4s.Ipv4Address
import scodec._
import scodec.bits._
import scodec.codecs._

sealed trait ProxyResponse
object ProxyResponse {
  final case class Socks4AuthorizationOk() extends ProxyResponse
  final case class Socks4AuthorizationErr() extends ProxyResponse
  final case class Socks5AuthMethodAccepted(method: Int) extends ProxyResponse

  object Socks5AuthMethodAccepted {
    implicit val codec: Codec[Socks5AuthMethodAccepted] = {
        ("method" | uint8)
    }.as[Socks5AuthMethodAccepted]
  }


  class Socks4AuthorizationOkCodec extends Codec[Socks4AuthorizationOk] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Socks4AuthorizationOk]] = Attempt.failure(Err("call decode on Socks4AuthorizationOkCodec"))

    override def encode(value: Socks4AuthorizationOk): Attempt[BitVector] = Attempt.successful(hex"005a000000000000".bits)

    override def sizeBound: SizeBound = SizeBound.exact(8 * 8)
  }
  object Socks4AuthorizationOk {
    implicit val codec: Codec[Socks4AuthorizationOk] = new Socks4AuthorizationOkCodec()
  }


  class Socks4AuthorizationErrCodec extends Codec[Socks4AuthorizationErr] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Socks4AuthorizationErr]] = Attempt.failure(Err("call decode on Socks4AuthorizationOkCodec"))

    override def encode(value: Socks4AuthorizationErr): Attempt[BitVector] = Attempt.successful(hex"005b000000000000".bits)

    override def sizeBound: SizeBound = SizeBound.exact(8 * 8)
  }
  object Socks4AuthorizationErr {
    implicit val codec: Codec[Socks4AuthorizationErr] = new Socks4AuthorizationErrCodec()
  }

  implicit val codec: Codec[ProxyResponse] = discriminated[ProxyResponse]
    .by(uint8)
    .typecase(5, Socks5AuthMethodAccepted.codec)
}


sealed trait Proxy
object Proxy {
  implicit val ipv4Codec: Codec[Ipv4Address] = uint32 xmap (Ipv4Address.fromLong, _.toLong)
  implicit val codec: Codec[Proxy] = discriminated[Proxy]
    .by(uint8)
    .typecase(4, Codec[SocksV4])
    .typecase(5, Codec[SocksV5Authorization])

  final case class SocksV4(command: Socks4Command, port: Int, address: Ipv4Address, clientId: String, domain: Option[String]) extends Proxy
  final case class SocksV5Authorization(supportedAuthProtocolsCount: Int, authProtocols: Vector[Int]) extends Proxy
  final case class Socks5Header(command: Socks5Command, address: Socks5Address, port: Int) extends Proxy
  object HttpTunnel extends Proxy
  final case class Http(address: String) extends Proxy

  object SocksV4 {
    implicit val codec: Codec[SocksV4] = {
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

  object SocksV5Authorization {
    implicit val codec: Codec[SocksV5Authorization] = {
      (("supportedAuthProtocolsCount" | uint8) flatPrepend { count =>
        ("authProtocols" | vectorOfN(provide(count), uint8)).hlist
      })
    }.as[SocksV5Authorization]
  }

  object Socks5Header {
    implicit val codec: Codec[Socks5Header] = {
      ("command" | Codec[Socks5Command]) ::
        constant(hex"00") ::
        ("address" | Codec[Socks5Address]) ::
        ("port" | uint16)
    }.as[Socks5Header]
  }
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

sealed trait Socks5Address
object Socks5Address {
  implicit val codec: Codec[Socks5Address] = discriminated[Socks5Address]
    .by(uint8)
    .typecase(1, Proxy.ipv4Codec.xmap[IpV4](Socks5Address.IpV4, _.address))
    .typecase(3, variableSizeBytes(uint8, ascii).xmap[Domain](Socks5Address.Domain, _.domain))
  case class IpV4(address: Ipv4Address) extends Socks5Address
  case class Domain(domain: String) extends Socks5Address
}