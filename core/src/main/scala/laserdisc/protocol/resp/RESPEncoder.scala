package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESPEncoder._

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec

private[resp] trait RESPEncoder[A] { self =>
  def encode(a: A): Array[Byte]
  def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[A]

  final def biMap[B](f: A => B, g: B => A): RESPEncoder[B] =
    new RESPEncoder[B] {
      override def encode(b: B): Array[Byte] = self.encode(g(b))
      override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[B] =
        self.decode(raw).map(_.map(f))
    }

  final def biMap_[B](f: RESPDecodingStep[A] => RESPCodecErr | RESPDecodingStep[B], g: B => A): RESPEncoder[B] =
    new RESPEncoder[B] {
      override def encode(b: B): Array[Byte] = self.encode(g(b))
      override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[B] =
        self.decode(raw).flatMap(d => f(d))
    }

  final def encodeList(l: List[A]): Array[Byte] =
    l.map(encode).reduce(_ ++ _)

  final def decodeVector(raw: Array[Byte], expected: Long): RESPCodecErr | RESPDecodingStep[Vector[A]] = {
    var remaining                   = raw
    var soFar                       = Vector.empty[A]
    var error: Option[RESPCodecErr] = None

    while (remaining.nonEmpty && (soFar.length < expected) && error.isEmpty)
      self.decode(remaining) match {
        case Right(RESPDecodingStep(step, reminder)) =>
          soFar = soFar :+ step
          remaining = reminder
        case Left(err) =>
          error = Some(err)
      }

    if (error.isEmpty) Right(RESPDecodingStep(soFar, remaining))
    else Left(error.getOrElse(BBB("ERRORRRRRRR")))
  }

  final def nestedIn(layer: RESPEncoder[Array[Byte]]): RESPEncoder[A] =
    new RESPEncoder[A] {
      override def encode(a: A): Array[Byte] =
        layer.encode(self.encode(a))

      override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[A] =
        layer.decode(raw).flatMap { s =>
          self.decode(s.step).map(_.mapReminder(_ ++ s.remainder))
        }
    }

  final def toCrlfTerminated(crlfAfter: Int = 0): RESPEncoder[A] =
    nestedIn(terminatedFrom(crlf, crlfAfter))
}

private[resp] object RESPEncoder extends ByteArrayFunctions {
  final val plus              = "+".getBytes(UTF_8)
  final val minus             = "-".getBytes(UTF_8)
  final val colon             = ":".getBytes(UTF_8)
  final val dollar            = "$".getBytes(UTF_8)
  final val star              = "*".getBytes(UTF_8)
  final val crlf              = "\r\n".getBytes(UTF_8)
  final val minusOne          = "-1".getBytes(UTF_8)
  final val zero              = "0".getBytes(UTF_8)
  final val respCommandIdSize = 1

  sealed abstract class RESPCodecErr(val m: String) extends laserdisc.Platform.LaserDiscRuntimeError(m) {
    override def toString: String = m
  }
  final case class AAAAAAA(message: String) extends RESPCodecErr(message)
  final case class BBB(message: String)     extends RESPCodecErr(message)

  final def terminatedFrom(termination: Array[Byte], from: Int): RESPEncoder[Array[Byte]] =
    new RESPEncoder[Array[Byte]] {
      override final def encode(a: Array[Byte]): Array[Byte] = a ++ termination
      override final def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[Array[Byte]] = {
        val i = positionOfArray(raw)(termination, from)
        if (i == -1)
          Left(AAAAAAA(s"Does not contain 'CRLF' termination bytes. Content: ${tailToUtf8(raw)}"))
        else Right(RESPDecodingStep(raw.take(i), raw.drop(i + termination.length)))
      }
    }

  final val lenientUtf8Codec = new RESPEncoder[String] {
    override def encode(str: String): Array[Byte] = str.getBytes(UTF_8)
    override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[String] =
      Right(RESPDecodingStep(new String(raw, UTF_8), Array.empty))
    override def toString: String = s"lenient-${UTF_8.displayName}"
  }

  final val hexCodec = new RESPEncoder[String] {
    override def encode(str: String): Array[Byte] = str.getBytes(UTF_8)
    override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[String] =
      Right(RESPDecodingStep(raw.map(_.toInt.toHexString).mkString(" "), Array.empty))
    override def toString: String = s"lenient-${UTF_8.displayName}"
  }
}

private[resp] sealed trait ByteArrayFunctions {
  import RESPEncoder.lenientUtf8Codec

  final def same(array: Array[Byte], other: Array[Byte]): Boolean =
    array.sameElements(other)

  final def positionOfArray(array: Array[Byte])(of: Array[Byte], from: Int): Int = {
    @tailrec
    def loop(curr: Array[Byte], currIdx: Int): Int =
      if (curr.startsWith(of)) currIdx
      else if (curr.isEmpty) -1
      else loop(curr.tail, currIdx + 1)

    loop(array.drop(from), from)
  }

  final def consumeMap[B](bytes: Array[Byte])(n: Int, f: (Array[Byte], Array[Byte]) => RESPCodecErr | B): RESPCodecErr | B =
    if (bytes.length >= n) f(bytes.take(n), bytes.drop(n))
    else
      Left(
        BBB(
          s"RESPCodec consumeMap error: tried consuming too many bytes from input. Requested: $n, Input size: ${bytes.length}, Input: ${toUtf8(bytes)}"
        )
      )

  final def tailToUtf8(bytes: Array[Byte], take: Int = 48): String =
    lenientUtf8Codec.decode(bytes.takeRight(take)).fold(_ => "content is not UTF-8 encoded", _.step)

  final def toUtf8(bytes: Array[Byte], take: Int = 48): String =
    lenientUtf8Codec.decode(bytes.take(take)).fold(_ => "content is not UTF-8 encoded", _.step)

  final def toHex(bytes: Array[Byte], take: Int = 48): String =
    hexCodec.decode(bytes.take(take)).fold(_ => "content is not representable as Hex", _.step)
}
