package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESPCodec.toHex
import laserdisc.protocol.resp.RESPWireState.{Complete, CompleteWithRemainder, Incomplete, MissingBytes, stateOf}

import scala.annotation.tailrec

private[laserdisc] sealed trait RESPFrame extends Product with Serializable with EitherSyntax {
  def append(bytes: Array[Byte]): Exception | NonEmptyRESPFrame = nextFrame(bytes)
  protected final def nextFrame(bytes: Array[Byte]): Exception | NonEmptyRESPFrame =
    stateOf(bytes) match {
      case Right(MissingBytes(n))             => Right(IncompleteFrame(bytes, n))
      case Right(Incomplete)                  => Right(IncompleteFrame(bytes, 0L))
      case Right(Complete)                    => Right(CompleteFrame(bytes))
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(Vector(CompleteFrame(c)), r))
      case Left(e) =>
        Left(UnknownBufferState(s"Error building the frame from buffer: ${e.m}. Hex:${toHex(bytes)}"))
    }

  @tailrec private[this] final def consumeRemainder(current: MoreThanOneFrame): Exception | MoreThanOneFrame =
    stateOf(current.remainder) match {
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(current.complete :+ CompleteFrame(c), r))
      case Right(Complete)                    => Right(MoreThanOneFrame(current.complete :+ CompleteFrame(current.remainder), Array.empty))
      case Left(ee) =>
        Left(UnknownBufferState(s"Err building the frame from a remainder: ${ee.m}. Hex:${toHex(current.remainder)}"))
      case _ => Right(current)
    }
}

private[laserdisc] case object EmptyFrame        extends RESPFrame
private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame

private[laserdisc] final case class CompleteFrame(bytes: Array[Byte]) extends NonEmptyRESPFrame {
  override def equals(obj: Any): Boolean =
    obj match {
      case CompleteFrame(b) => RESPCodec.same(bytes, b)
      case _                => false
    }
}
private[laserdisc] final case class MoreThanOneFrame(private[laserdisc] val complete: Vector[CompleteFrame], remainder: Array[Byte])
    extends NonEmptyRESPFrame {
  override def equals(obj: Any): Boolean =
    obj match {
      case MoreThanOneFrame(c, r) => (complete == c) && RESPCodec.same(remainder, r)
      case _                      => false
    }
}
private[laserdisc] final case class IncompleteFrame(partial: Array[Byte], bytesToComplete: Long) extends NonEmptyRESPFrame {
  override def append(bytes: Array[Byte]): Exception | NonEmptyRESPFrame =
    //  Saves some size checks
    if (bytesToComplete > 0 && bytesToComplete == bytes.length) Right(CompleteFrame(partial ++ bytes))
    else nextFrame(partial ++ bytes)

  override def equals(obj: Any): Boolean =
    obj match {
      case IncompleteFrame(p, tc) => RESPCodec.same(partial, p) && (bytesToComplete == tc)
      case _                      => false
    }
}

private[laserdisc] final case class UnknownBufferState(message: String) extends laserdisc.Platform.LaserDiscRespFrameError(message)
