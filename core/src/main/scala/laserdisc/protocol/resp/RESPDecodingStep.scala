package laserdisc
package protocol
package resp

private[resp] final case class RESPDecodingStep[+A](step: A, remainder: Array[Byte]) {
  def flatMap[B](f: A => RESPDecodingStep[B]): RESPDecodingStep[B] = f(step)

  def map[B](f: A => B): RESPDecodingStep[B] =
    RESPDecodingStep(f(step), remainder)

  def mapReminder(f: Array[Byte] => Array[Byte]): RESPDecodingStep[A] =
    RESPDecodingStep(step, f(remainder))
}
