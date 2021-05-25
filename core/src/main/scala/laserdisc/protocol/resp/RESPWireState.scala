package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESP.reprOfInt
import laserdisc.protocol.resp.RESPCodec._
import laserdisc.protocol.resp.RESPWireState.{Complete, CompleteWithRemainder, Incomplete, MissingBytes}

import scala.annotation.tailrec

private[resp] sealed trait RESPWireState extends Product with Serializable

object RESPWireState extends RESPWireStateFunctions {
  type Incomplete = Incomplete.type
  type Complete   = Complete.type

  final case object Complete   extends RESPWireState
  final case object Incomplete extends RESPWireState

  final case class MissingBytes(stillToReceive: Long) extends RESPWireState

  final case class CompleteWithRemainder(complete: Array[Byte], remainder: Array[Byte]) extends RESPWireState {
    override def equals(obj: Any): Boolean =
      obj match {
        case CompleteWithRemainder(c, r) => RESPCodec.same(complete, c) && RESPCodec.same(remainder, r)
        case _                           => false
      }
  }
}

private[resp] trait RESPWireStateFunctions extends EitherSyntax {

  final val stateOf: Array[Byte] => RESPCodecErr | RESPWireState = bytes =>
    readDiscriminator(bytes) { case (rd, payload) =>
      if (same(rd, `plus`) || same(rd, `minus`) || same(rd, `colon`)) {
        val eomIndex = positionOfArray(bytes)(crlf, from = 0)
        val size     = eomIndex + crlf.size

        if (eomIndex == -1) Right(Incomplete)
        else if (size < bytes.length) Right(CompleteWithRemainder(bytes.take(size), bytes.drop(size)))
        else Right(Complete)
      } else if (same(rd, `dollar`))
        evalWithSizeDecodedFrom(payload) {
          case Left(_) => Incomplete
          case Right(RESPDecodingStep(value, remainder)) =>
            val decoded  = respCommandIdSize + value.bytes.length + crlf.size
            val expected = value.decoded + crlf.size
            val size     = decoded + expected

            if (value.decoded >= 0 && remainder.length == expected) Complete
            else if (value.decoded >= 0 && remainder.length > expected) CompleteWithRemainder(bytes.take(size), bytes.drop(size))
            else if (value.decoded == -1 && remainder.isEmpty) Complete
            else if (value.decoded == -1 && remainder.nonEmpty) CompleteWithRemainder(bytes.take(decoded), bytes.drop(decoded))
            else MissingBytes(expected.toLong - remainder.length)
        }
      else if (same(rd, `star`))
        attemptEvalWithSizeDecodedFrom(payload) {
          case Left(_) => Right(Incomplete)
          case Right(RESPDecodingStep(value, remainder)) =>
            val size = respCommandIdSize + value.bytes.length + crlf.size

            if (value.decoded == -1 && remainder.isEmpty) Right(Complete)
            else if (value.decoded == -1 && remainder.nonEmpty) Right(CompleteWithRemainder(bytes.take(size), bytes.drop(size)))
            else stateOfArr(value.decoded, remainder, bytes.take(size))
        }
      else Left(BBB(s"unidentified RESP type when checking the state. Was:${toUtf8(rd)} (Hex:${toHex(rd)}), Input Hex: ${toHex(bytes)}"))
    }

  @tailrec
  private[this] final def stateOfArr(missing: Int, remainder: Array[Byte], bytes: Array[Byte]): RESPCodecErr | RESPWireState =
    missing match {
      case 0L =>
        if (remainder.isEmpty) Right(Complete)
        else Right(CompleteWithRemainder(bytes, remainder))

      case _ if remainder.isEmpty => Right(Incomplete)

      case _ =>
        stateOf(remainder) match {
          case Right(CompleteWithRemainder(c, r)) => stateOfArr(missing - 1, r, bytes ++ c)
          case Right(Complete) if missing == 1    => Right(Complete)
          case Right(Complete)                    => Right(Incomplete)
          case Right(incomplete)                  => Right(incomplete)
          case left                               => left
        }
    }

  private[this] final def readDiscriminator(bytes: Array[Byte])(
      f: (Array[Byte], Array[Byte]) => RESPCodecErr | RESPWireState
  ): RESPCodecErr | RESPWireState =
    consumeMap(bytes)(respCommandIdSize, (a, b) => f(a, b))

  private[this] final def evalWithSizeDecodedFrom[A](bytes: Array[Byte])(
      f: (Incomplete | RESPDecodingStep[Repr[Int]]) => A
  ): RESPCodecErr | A =
    attemptEvalWithSizeDecodedFrom(bytes)(x => Right(f(x)))

  private[this] final def attemptEvalWithSizeDecodedFrom[A](bytes: Array[Byte])(
      f: (Incomplete | RESPDecodingStep[Repr[Int]]) => RESPCodecErr | A
  ): RESPCodecErr | A =
    reprOfInt
      .toCrlfTerminated()
      .decode(bytes)
      .fold(
        {
          case AAAAAAA(_) => f(Left(Incomplete))
          case err        => Left(err)
        },
        res => f(Right(res))
      )
}
