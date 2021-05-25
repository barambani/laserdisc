//package laserdisc
//package protocol
//
//import laserdisc.protocol.BitVectorDecoding._
//import laserdisc.protocol.RESP.stateOf
//
//import java.nio.charset.StandardCharsets.UTF_8
//import scala.annotation.tailrec
//
//private[laserdisc] sealed trait RESPFrame extends Product with Serializable with EitherSyntax with BitVectorSyntax {
//  def append(bytes: Array[Byte]): Exception | NonEmptyRESPFrame = nextFrame(bytes)
//  protected final def nextFrame(bytes: Array[Byte]): Exception | NonEmptyRESPFrame =
//    stateOf(bytes) match {
//      case Right(MissingBits(n))              => Right(IncompleteFrame(bytes, n))
//      case Right(Incomplete)                  => Right(IncompleteFrame(bytes, 0L))
//      case Right(Complete)                    => Right(CompleteFrame(bytes))
//      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(Vector(CompleteFrame(c)), r))
//      case Left(e)                            => Left(UnknownBufferState(s"Err building the frame from buffer: $e. Content: ${new String(bytes, UTF_8)}"))
//    }
//
//  @tailrec private[this] final def consumeRemainder(current: MoreThanOneFrame): Exception | MoreThanOneFrame =
//    stateOf(current.remainder) match {
//      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(current.complete :+ CompleteFrame(c), r))
//      case Right(Complete)                    => Right(MoreThanOneFrame(current.complete :+ CompleteFrame(current.remainder), Array.empty))
//      case Left(ee) =>
//        Left(UnknownBufferState(s"Err building the frame from a remainder: $ee. Content: ${new String(current.remainder, UTF_8)}"))
//      case _ => Right(current)
//    }
//}
//
//private[laserdisc] case object EmptyFrame        extends RESPFrame
//private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame
//
//private[laserdisc] final case class CompleteFrame(bytes: Array[Byte]) extends NonEmptyRESPFrame
//private[laserdisc] final case class MoreThanOneFrame(private[laserdisc] val complete: Vector[CompleteFrame], remainder: Array[Byte])
//    extends NonEmptyRESPFrame
//private[laserdisc] final case class IncompleteFrame(partial: Array[Byte], bitsToComplete: Long) extends NonEmptyRESPFrame {
//  override def append(bytes: Array[Byte]): Exception | NonEmptyRESPFrame =
//    //  Saves some size checks
//    if (bitsToComplete > 0 && bitsToComplete == bytes.length) Right(CompleteFrame(partial ++ bytes))
//    else nextFrame(partial ++ bytes)
//}
//
//private[laserdisc] final case class UnknownBufferState(message: String) extends laserdisc.Platform.LaserDiscRespFrameError(message)
