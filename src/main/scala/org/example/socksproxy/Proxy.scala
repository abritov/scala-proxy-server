package org.example.socksproxy

import com.comcast.ip4s.Ipv4Address
import scodec._
import scodec.bits._
import scodec.codecs._

sealed trait ProxyResponse
object ProxyResponse {
  final case class Socks4Response(code: Socks4ResponseCode) extends ProxyResponse
  final case class Socks5AuthMethodAccepted(method: Int) extends ProxyResponse
  final case class Socks5Response(code: Socks5ResponseCode, addressType: Socks5Address, port: Int) extends ProxyResponse


  implicit val codec: Codec[ProxyResponse] = discriminated[ProxyResponse]
    .by(uint8)
    .typecase(0, Codec[Socks4Response])
    .typecase(5, Codec[Socks5AuthMethodAccepted])
    .typecase(5, Codec[Socks5Response])
    .encodeOnly

  object Socks4Response {
    implicit val codec: Codec[Socks4Response] = {
        ("code" | Codec[Socks4ResponseCode]) ::
        constant(hex"000000000000")
    }.as[Socks4Response]

    def requestGranted: Socks4Response = Socks4Response(Socks4ResponseCode.RequestGranted)
    def requestDenied: Socks4Response = Socks4Response(Socks4ResponseCode.RequestDenied)
  }

  sealed trait Socks4ResponseCode
  object Socks4ResponseCode {
    implicit val codec: DiscriminatorCodec[Socks4ResponseCode, Int] = mappedEnum(
      uint8,
      Socks4ResponseCode.RequestGranted -> 0x5a,
      Socks4ResponseCode.RequestDenied -> 0x5b,
    )
    object RequestGranted extends Socks4ResponseCode
    object RequestDenied extends Socks4ResponseCode
  }

  object Socks5AuthMethodAccepted {
    implicit val codec: Codec[Socks5AuthMethodAccepted] = {
        ("method" | uint8)
    }.as[Socks5AuthMethodAccepted]
  }

  object Socks5Response {
    implicit val codec: Codec[Socks5Response] = {
      ("response" | Codec[Socks5ResponseCode]) ::
        constant(hex"00") ::
        ("addressType" | Codec[Socks5Address]) ::
        ("port" | uint16)
    }.as[Socks5Response]

    def requestGranted(addressType: Socks5Address, port: Int): Socks5Response = Socks5Response(Socks5ResponseCode.RequestGranted, addressType, port)
    def endpointUnavailable(addressType: Socks5Address, port: Int): Socks5Response = Socks5Response(Socks5ResponseCode.EndpointUnavailable, addressType, port)
    def notSupported(addressType: Socks5Address, port: Int): Socks5Response = Socks5Response(Socks5ResponseCode.NotSupported, addressType, port)
  }

  sealed trait Socks5ResponseCode
  object Socks5ResponseCode {
    implicit val codec: DiscriminatorCodec[Socks5ResponseCode, Int] = mappedEnum(
      uint8,
      Socks5ResponseCode.RequestGranted -> 0,
      Socks5ResponseCode.ServerError -> 1,
      Socks5ResponseCode.Forbidden -> 2,
      Socks5ResponseCode.NetworkUnavailable -> 3,
      Socks5ResponseCode.EndpointUnavailable -> 4,
      Socks5ResponseCode.ConnectionRefused -> 5,
      Socks5ResponseCode.TTLTimeout -> 6,
      Socks5ResponseCode.NotSupported -> 7,
      Socks5ResponseCode.AddressNotSupported -> 8
    )
    object RequestGranted extends Socks5ResponseCode
    object ServerError extends Socks5ResponseCode
    object Forbidden extends Socks5ResponseCode
    object NetworkUnavailable extends Socks5ResponseCode
    object EndpointUnavailable extends Socks5ResponseCode
    object ConnectionRefused extends Socks5ResponseCode
    object TTLTimeout extends Socks5ResponseCode
    object NotSupported extends Socks5ResponseCode
    object AddressNotSupported extends Socks5ResponseCode
  }
}


sealed trait Proxy
object Proxy {
  implicit val ipv4Codec: Codec[Ipv4Address] = uint32 xmap (Ipv4Address.fromLong, _.toLong)
  implicit val codec: Codec[Proxy] = discriminated[Proxy]
    .by(uint8)
    .typecase(4, Codec[SocksV4])
    .typecase(5, Codec[SocksV5])

  final case class SocksV4(command: Socks4Command, port: Int, address: Ipv4Address, clientId: String, domain: Option[String]) extends Proxy
  final case class SocksV5(auth: Option[SocksV5Authorization], header: Option[Socks5Header]) extends Proxy
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

  object SocksV5 {
    implicit val codec: Codec[SocksV5] = {
      def decodeRecord(bits: BitVector): Attempt[DecodeResult[SocksV5]] = Socks5Header.codec.decode(bits)
        .fold(
          _ => SocksV5Authorization.codec.decode(bits).fold(
            _ => Attempt.failure(Err("invalid socks5 decode state2")),
            { case DecodeResult(auth, rest3) => Attempt.successful(DecodeResult(SocksV5(Some(auth), None), rest3)) }),
          {
            case DecodeResult(header, rest2) => Attempt.successful(DecodeResult(SocksV5(None, Some(header)), rest2))
            case _ => Attempt.failure(Err("invalid socks5 decode state1"))
          })

      def encodeRecord(rec: SocksV5): Attempt[BitVector] = rec match {
        case SocksV5(Some(auth), None) => SocksV5Authorization.codec.encode(auth)
        case SocksV5(None, Some(header)) => Socks5Header.codec.encode(header)
      }

      Codec(Encoder(encodeRecord _), Decoder(decodeRecord _))
    }
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
  final case class IpV4(address: Ipv4Address) extends Socks5Address
  final case class Domain(domain: String) extends Socks5Address
}

final case class SocksV5Authorization(supportedAuthProtocolsCount: Int, authProtocols: Vector[Int])

object SocksV5Authorization {
  implicit val codec: Codec[SocksV5Authorization] = {
    (("supportedAuthProtocolsCount" | uint8) flatPrepend { count =>
      ("authProtocols" | vectorOfN(provide(count), uint8)).hlist
    })
  }.as[SocksV5Authorization]
}

final case class Socks5Header(command: Socks5Command, address: Socks5Address, port: Int)

object Socks5Header {
  implicit val codec: Codec[Socks5Header] = {
    ("command" | Codec[Socks5Command]) ::
      constant(hex"00") ::
      ("address" | Codec[Socks5Address]) ::
      ("port" | uint16)
  }.as[Socks5Header]
}