package laserdisc
package protocol
package resp

import shapeless.Generic

/** [[https://redis.io/topics/protocol Redis Protocol Specification]]
  *
  * This sealed trait represents the entire Redis Serialization Protocol algebra
  *
  * Concrete instances of this trait must be created using this trait's companion
  * object's methods, were [[scodec.Codec]]s for each are also defined
  *
  * @see [[RESPCodecs]]
  */
sealed trait RESP extends AnyRef with Serializable
object RESP       extends RESPCodecs with RESPCoproduct

/** RESP [[https://redis.io/topics/protocol#resp-simple-strings Simple Strings]]
  *
  * @note Sometimes the value "OK" is used to represent a successful
  * acknowledgement/processing of a command.
  *
  * @example
  * {{{
  *   val s: Str = Str("some string")
  * }}}
  *
  * @param value The wrapped string value
  */
final case class Str(value: String) extends RESP
object Str {
  final def apply[A](a: A)(implicit A: Show[A]): Str = new Str(A.show(a))
}

/** RESP [[https://redis.io/topics/protocol#resp-errors Errors]]
  *
  * RESP [[Err]]s are also [[scala.RuntimeException]]s, although
  * __where possible__ they will not contain stacktrace data
  *
  * @example
  * {{{
  *   val e: Err = Err("some error message")
  * }}}
  * @param message The wrapped exception's message
  */
final case class Err(message: String) extends laserdisc.Platform.LaserDiscRuntimeError(message) with RESP

/** RESP [[https://redis.io/topics/protocol#resp-integers Integers]]
  *
  * @note Sometimes the values 0 and 1 are used to represent boolean
  * values. In this case 0 corresponds to False while 1 to True,
  * respectively.
  *
  * @example
  * {{{
  *   val n: Num = Num(42)
  * }}}
  *
  * @param value The wrapped long value
  */
final case class Num(value: Long) extends RESP

/** RESP [[https://redis.io/topics/protocol#resp-bulk-strings Bulk Strings]]
  *
  * There can be 2 cases:
  *  - `null` bulk strings, where the length is -1 and no actual underlying string is present
  *  - actual (non-null) bulk strings, where the length is >= 0
  *
  * @example
  * {{{
  *   val b: Bulk      = Bulk("some string")
  *   val nb: NullBulk = NullBulk
  * }}}
  * @see [[Show]]
  */
sealed trait GenBulk extends RESP
case object NullBulk extends GenBulk

/** This is the special case of a non-null RESP [[GenBulk]]
  *
  * @param value The wrapped bulk string value
  */
final case class Bulk(value: String) extends GenBulk
object Bulk {
  final def apply[A](a: A)(implicit A: Show[A]): Bulk = new Bulk(A.show(a))

  implicit final val bulkShow: Show[Bulk] = Show.instance(_.value)
}

/** RESP [[https://redis.io/topics/protocol#resp-arrays Arrays]]
  *
  * There can be 2 cases:
  *  - `nil` arrays, where the length is -1 and no array element is present
  *  - actual (non-nil) arrays, where the length is >= 0
  *
  * @note [[[Arr#apply(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* Arr#apply(one: RESP, rest: RESP*)]]]
  * is an overload which supports the creation of guaranteed non-empty
  * sequences only. This is achieved through the usage of one fixed
  * parameter followed by a var-arg of the same
  * @example
  * {{{
  *   val arr: Arr                = Arr(List(Str("hello"), Str("world")))
  *   val guaranteedNonEmpty: Arr = Arr(Str("hello"), Str("world"))
  *   val empty: Arr              = Arr(List.empty)
  *   val nil: NilArr             = NilArr
  * }}}
  */
sealed trait GenArr extends RESP
case object NilArr  extends GenArr

/** This is the special case of a non-nil RESP [[GenArr]]
  *
  * These can be constructed either by the default case class' apply by resorting to the overloaded
  * [[[Arr#apply(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* Arr#apply(one: RESP, rest: RESP*)]]]
  * method which expects one parameter to be supplied followed
  * by a (possibly empty) sequence of [[RESP]]s (vararg).
  *
  * @param elements The wrapped array values, as a [[scala.List]] of [[RESP]]
  */
final case class Arr(elements: List[RESP]) extends GenArr {
  override def toString: String = s"Arr(${elements.mkString(COMMA)})"
}
object Arr {
  final def apply(one: RESP, rest: RESP*): Arr = new Arr(one +: rest.toList)
}

final case class RESPDecErr(message: String) extends laserdisc.Platform.LaserDiscRespProtocolDecodingError(message)

sealed trait RESPCoproduct {
  final val gen = Generic[RESP]

  final type RESPCoproduct = gen.Repr
}
