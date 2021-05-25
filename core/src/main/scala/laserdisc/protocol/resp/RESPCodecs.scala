package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESPCodec._

import java.{lang => j}

private[resp] final case class Repr[A](decoded: A, bytes: Array[Byte])

private[resp] trait RESPCodecs extends {

  private[this] final val crlfTerminatedString: RESPCodec[String] =
    lenientUtf8Codec.toCrlfTerminated()
  private[this] final def crlfTerminatedStringAt(crlfAfter: Int): RESPCodec[String] =
    lenientUtf8Codec.toCrlfTerminated(crlfAfter)

  private[this] final val crlfTerminatedLong: RESPCodec[Long] = crlfTerminatedString.biMap_[Long](
    res =>
      try Right(RESPDecodingStep(j.Long.parseLong(res.step), res.remainder))
      catch { case _: NumberFormatException => Left(BBB(s"Expected long but found ${res.step}")) },
    _.toString
  )

  private[this] final val strCodec: RESPCodec[Str] = crlfTerminatedString.biMap[Str](Str.apply, _.value)
  private[this] final val errCodec: RESPCodec[Err] = crlfTerminatedString.biMap[Err](Err.apply, _.message)
  private[this] final val numCodec: RESPCodec[Num] = crlfTerminatedLong.biMap[Num](Num.apply, _.value)

  private[this] final val bulkCodec: RESPCodec[GenBulk] = new RESPCodec[GenBulk] {
    private[this] final val nullBulkBytes          = minusOne ++ crlf
    private[this] final def failDec(negSize: Long) = BBB(s"failed to decode bulk-string of size $negSize")

    override final def encode(bulk: GenBulk): Array[Byte] =
      bulk match {
        case NullBulk => nullBulkBytes
        case Bulk(s) =>
          val encodedString = crlfTerminatedString.encode(s)
          val encodedLength = crlfTerminatedLong.encode(encodedString.length.toLong - crlf.length)
          encodedLength ++ encodedString
      }

    override final def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[GenBulk] =
      crlfTerminatedLong.decode(raw).flatMap { res =>
        res.step match {
          case -1 => Right(RESPDecodingStep(NullBulk, res.remainder))
          case size if size >= 0 =>
            crlfTerminatedStringAt(crlfAfter = size.toInt)
              .biMap[Bulk](Bulk.apply, b => b.value)
              .decode(res.remainder)
          case negSize => Left(failDec(negSize))
        }
      }
  }

  private[this] final val arrCodec: RESPCodec[GenArr] = new RESPCodec[GenArr] {
    private[this] final val nilArrBits   = minusOne ++ crlf
    private[this] final val emptyArrBits = zero ++ crlf
    private[this] final def checkSize(
        v: Vector[RESP],
        expectedSize: Long,
        rem: Array[Byte]
    ): RESPCodecErr | RESPDecodingStep[Vector[RESP]] =
      if (v.length == expectedSize) Right(RESPDecodingStep(v, rem))
      else Left(BBB(s"Insufficient number of elements: decoded ${v.size} instead of $expectedSize"))
    private[this] final def failDec(negSize: Long) = BBB(s"failed to decode array of size $negSize")

    override final def encode(arr: GenArr): Array[Byte] =
      arr match {
        case NilArr              => nilArrBits
        case Arr(v) if v.isEmpty => emptyArrBits
        case Arr(v) =>
          val encodedLength  = crlfTerminatedLong.encode(v.length.toLong)
          val encodedContent = respCodec.encodeList(v)
          encodedLength ++ encodedContent
      }

    override final def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[GenArr] =
      crlfTerminatedLong.decode(raw).flatMap { res =>
        res.step match {
          case -1 => Right(RESPDecodingStep(NilArr, res.remainder))
          case 0  => Right(RESPDecodingStep(Arr(List.empty), res.remainder))
          case size if size >= 0 =>
            respCodec
              .decodeVector(res.remainder, size)
              .flatMap(v => checkSize(v.step, size, v.remainder))
              .map(_.map(respV => Arr(respV.toList)))
          case negSize => Left(failDec(negSize))
        }
      }
  }

  final val reprOfInt: RESPCodec[Repr[Int]] =
    new RESPCodec[Repr[Int]] {
      override def encode(a: Repr[Int]): Array[Byte] = a.bytes
      override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[Repr[Int]] =
        try lenientUtf8Codec.decode(raw).map(_.map(strInt => Repr(strInt.toInt, raw)))
        catch { case _: NumberFormatException => Left(BBB(s"Expected long but found ${toUtf8(raw)}")) }
    }

  final val respCodec: RESPCodec[RESP] = new RESPCodec[RESP] {
    override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[RESP] =
      consumeMap(raw)(
        respCommandIdSize,
        { case (chunk, remainder) =>
          if (same(chunk, `plus`)) strCodec.decode(remainder)
          else if (same(chunk, `minus`)) errCodec.decode(remainder)
          else if (same(chunk, `colon`)) numCodec.decode(remainder)
          else if (same(chunk, `dollar`)) bulkCodec.decode(remainder)
          else if (same(chunk, `star`)) arrCodec.decode(remainder)
          else Left(BBB(s"unidentified RESP type decoding RESP. Was:${toUtf8(chunk)} (Hex:${toHex(chunk)}), Input: ${toUtf8(raw)}"))
        }
      )

    override def encode(a: RESP): Array[Byte] =
      a match {
        case str: Str      => plus ++ strCodec.encode(str)
        case err: Err      => minus ++ errCodec.encode(err)
        case num: Num      => colon ++ numCodec.encode(num)
        case bulk: GenBulk => dollar ++ bulkCodec.encode(bulk)
        case arr: GenArr   => star ++ arrCodec.encode(arr)
      }

    override final val toString: String = "RESP"
  }
}
