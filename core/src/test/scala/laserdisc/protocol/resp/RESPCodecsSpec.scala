package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESP.respCodec
import laserdisc.protocol.resp.RESPEncoder.{RESPCodecErr, lenientUtf8Codec}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

import java.nio.charset.StandardCharsets.UTF_8

object RESPCodecsSpec {
  private[this] final object functions {
    final val stringToBytesLength = (s: String) => s.getBytes(UTF_8).length
    final val stringToResp        = (s: String) => respCodec.decode(s.getBytes(UTF_8))
    final val respToString        = (resp: RESP) => lenientUtf8Codec.decode(respCodec.encode(resp)).fold(_.m, _.step)
    final val respSeqToString     = (xs: Seq[RESP]) => xs.map(respToString).mkString
    final val roundTripAttempt    = (resp: RESP) => respCodec.decode(respCodec.encode(resp))
  }

  private implicit final class RichChar(private val underlying: Char) extends AnyVal {
    def toHex: String = underlying.toByte.toInt.toHexString
  }

  private implicit final class RichString(private val underlying: String) extends AnyVal {
    def RESP: RESPCodecErr | RESP = functions.stringToResp(underlying).map(_.step)
    def bytesLength: Int          = functions.stringToBytesLength(underlying)
  }

  private implicit final class RichRESP(private val underlying: RESP) extends AnyVal {
    def wireFormat: String = functions.respToString(underlying)
    def roundTrip: RESP    = functions.roundTripAttempt(underlying).fold(err => throw new Exception(err.m), _.step)
  }

  private implicit final class RichSeqRESP(private val underlying: Seq[RESP]) extends AnyVal {
    def wireFormat: String = functions.respSeqToString(underlying)
  }
}

final class RESPCodecsSpec extends BaseSpec with EitherSyntax {
  import RESPCodecsSpec._

  private[this] val smallNumGen: Gen[Int] = chooseNum(0, 20)
  private[this] val invalidProtocolGen: Gen[Char] = {
    val exclusions = List('+', '-', ':', '$', '*')
    choose[Char](0, 127).suchThat(!exclusions.contains(_))
  } :| "invalid protocol discriminator"
  private[this] val stringGen: Gen[String] = listOf(utf8BMPCharGen).map(_.mkString) :| "string"
  private[this] val strGen: Gen[Str]       = stringGen.map(Str.apply) :| "simple string RESP"
  private[this] val errGen: Gen[Err]       = stringGen.map(Err.apply) :| "error RESP"
  private[this] val numGen: Gen[Num]       = arbitrary[Long].map(Num.apply) :| "integer RESP"
  private[this] val genBulkGen: Gen[GenBulk] = option(stringGen).map {
    case None    => NullBulk
    case Some(s) => Bulk(s)
  } :| "bulk string RESP"
  private[this] def genArrGen: Gen[GenArr] =
    smallNumGen.flatMap { size =>
      option(listOfN(size, respGen)).map {
        case None    => NilArr
        case Some(v) => Arr(v)
      }
    } :| "array RESP"
  private[this] def respGen: Gen[RESP] =
    lzy {
      frequency(2 -> strGen, 1 -> errGen, 2 -> numGen, 4 -> genBulkGen, 1 -> genArrGen)
    } :| "RESP"
  private[this] def respListGen: Gen[List[RESP]] = smallNumGen.flatMap(listOfN(_, respGen)) :| "list of RESPs"

  private[this] implicit val stringArb: Arbitrary[String]        = Arbitrary(stringGen)
  private[this] implicit val invalidProtocolArb: Arbitrary[Char] = Arbitrary(invalidProtocolGen)
  private[this] implicit val strArb: Arbitrary[Str]              = Arbitrary(strGen)
  private[this] implicit val errArb: Arbitrary[Err]              = Arbitrary(errGen)
  private[this] implicit val numArb: Arbitrary[Num]              = Arbitrary(numGen)
  private[this] implicit val genBulkArb: Arbitrary[GenBulk]      = Arbitrary(genBulkGen)
  private[this] implicit val genArrArb: Arbitrary[GenArr]        = Arbitrary(genArrGen)
  private[this] implicit val respListArb: Arbitrary[List[RESP]]  = Arbitrary(respListGen)

  property("A RESP codec handling unknown protocol type fails with correct error message") {
    forAll { c: Char =>
      assertLeftEquals(
        s"$c".RESP.leftMap(_.m),
        s"unidentified RESP type decoding RESP. Was:$c (Hex:${c.toHex}), Input: $c"
      )
    }
  }

  property("A RESP codec handling simple strings decodes them correctly") {
    forAll { s: String => assertEquals(s"+$s$CRLF".RESP, Str(s)) }
  }

  property("A RESP codec handling simple strings wire format decodes them correctly") {
    forAll { s: Str => assertEquals(s.wireFormat, s"+${s.value}$CRLF") }
  }

  property("A RESP codec handling simple strings roundtrips with no errors") {
    forAll { s: Str => assertEquals(s.roundTrip, s) }
  }

  property("A RESP codec handling errors decodes them correctly") {
    forAll { s: String => assertEquals(s"-$s$CRLF".RESP, Err(s)) }
  }

  property("A RESP codec handling errors encodes them correctly") {
    forAll { e: Err => assertEquals(e.wireFormat, s"-${e.message}$CRLF") }
  }

  property("A RESP codec handling errors roundtrips with no errors") {
    forAll { e: Err => assertEquals(e.roundTrip, e) }
  }

  property("A RESP codec handling integers decodes them correctly") {
    forAll { l: Long => assertEquals(s":$l$CRLF".RESP, Num(l)) }
  }

  property("A RESP codec handling integers encodes them correctly") {
    forAll { n: Num => assertEquals(n.wireFormat, s":${n.value}$CRLF") }
  }

  property("A RESP codec handling integers roundtrips with no errors") {
    forAll { n: Num => assertEquals(n.roundTrip, n) }
  }

  property("A RESP codec handling bulk strings fails with correct error message when decoding size < -1") {
    assertLeftEquals(
      s"$$-2${CRLF}bla$CRLF".RESP.leftMap(_.m),
      "failed to decode bulk-string of size -2"
    )
  }
  override val scalaCheckInitialSeed = "PTH3mWWxncNpyVMVMmwHe7k69xqSQaQQ8UFal4X-AiB="
  property("A RESP codec handling bulk strings decodes them correctly") {
    forAll { os: Option[String] =>
      os match {
        case None    => assertEquals(s"$$-1$CRLF".RESP, NullBulk)
        case Some(s) => assertEquals(s"$$${s.bytesLength}$CRLF$s$CRLF".RESP, Bulk(s))
      }
    }
  }

  property("A RESP codec handling bulk strings encodes them correctly") {
    forAll { b: GenBulk =>
      b match {
        case NullBulk => assertEquals(b.wireFormat, s"$$-1$CRLF")
        case Bulk(bs) => assertEquals(b.wireFormat, s"$$${bs.bytesLength}$CRLF$bs$CRLF")
      }
    }
  }

  property("A RESP codec handling bulk strings roundtrips with no errors") {
    forAll { b: GenBulk => assertEquals(b.roundTrip, b) }
  }

  property("A RESP codec handling arrays fails with correct error message when decoding size < -1") {
    assertLeftEquals(
      s"*-2${CRLF}bla$CRLF".RESP.leftMap(_.m),
      "failed to decode array of size -2"
    )
  }

  property("A RESP codec handling Arr strings decodes them correctly") {
    forAll { ors: Option[List[RESP]] =>
      ors match {
        case None     => assertEquals(s"*-1$CRLF".RESP, NilArr)
        case Some(xs) => assertEquals(s"*${xs.length}$CRLF${xs.wireFormat}".RESP, Arr(xs))
      }
    }
  }

  property("A RESP codec handling bulk strings encodes them correctly") {
    forAll { a: GenArr =>
      a match {
        case NilArr  => assertEquals(a.wireFormat, s"*-1$CRLF")
        case Arr(xs) => assertEquals(a.wireFormat, s"*${xs.length}$CRLF${xs.wireFormat}")
      }
    }
  }

  property("A RESP codec handling Arr strings roundtrips with no errors") {
    forAll { a: GenArr => assertEquals(a.roundTrip, a) }
  }
}
