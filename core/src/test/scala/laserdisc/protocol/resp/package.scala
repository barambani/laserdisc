package laserdisc
package protocol

package object resp {

  final def printEscaped(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  final def printEscapedArray(bytes: Array[Byte]): String =
    printEscaped(RESPEncoder.toUtf8(bytes, take = 10000))
}
