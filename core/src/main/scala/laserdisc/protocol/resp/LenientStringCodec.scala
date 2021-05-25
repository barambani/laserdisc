//package laserdisc
//package protocol
//package resp
//
//import laserdisc.protocol.resp.RESPCodec.RESPCodecErr
//
//import java.nio.charset.Charset
//
//private[resp] final case class LenientStringCodec(private[resp] val charset: Charset) extends RESPCodec[String] {
//  override def encode(str: String): Array[Byte] = str.getBytes(charset)
//  override def decode(raw: Array[Byte]): RESPCodecErr | RESPDecodingStep[String] =
//    Right(RESPDecodingStep(new String(raw, charset), Array.empty))
//  override def toString: String = s"lenient-${charset.displayName}"
//}
