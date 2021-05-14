package org.example.socksproxy

import com.comcast.ip4s.Ipv4Address
import scodec.Codec
import scodec.bits.HexStringSyntax

object Main extends App {
  val socks4 = Proxy.SocksV4(Socks4Command.Connect, 65535, Ipv4Address.fromBytes(127, 0, 0, 1), "test", None)

  println(Codec[Proxy].decode(hex"040100507F0000010461616161".bits))
  println(Codec[Proxy].decode(hex"05010001000304616161610050".bits))

  //  val x: Codec[(Int, ByteVector)] = uint8 flatZip { numBytes => bytes(numBytes) }
  //  val y: Codec[ByteVector] = x xmap[ByteVector]({ case (_, bv) => bv }, bv => (bv.size.toInt, bv))
  //  val result2 = y.decode(hex"040100507F0000010461616161".bits)
  //  println(result2)
  //
  //  val socks4Codec = uint8 ~ uint16 ~ uint32 ~ variableSizeBytes(uint8, ascii)
  //  val test = uint8 flatZip bytes xmap[Proxy]({ case (version, rest) =>
  //    version match {
  //      case 4 => socks4
  //    }
  //  },
  //    (proxy: Proxy) => {
  //      case SocksV4(command, port, address, clientId) => (4, socks4Codec.encode(((command.toByte.toInt, port), 0), clientId))
  //      case _ => throw new Error("encode unknown proxy type")
  //    }
  //  )
  //  println(result2)
}
