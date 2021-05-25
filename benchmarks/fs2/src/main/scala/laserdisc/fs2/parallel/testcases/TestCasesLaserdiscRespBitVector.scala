package laserdisc
package fs2
package parallel
package testcases

import cats.effect.kernel.Concurrent
import laserdisc.protocol.resp
import scodec.bits.BitVector

private[fs2] object TestCasesLaserdiscRespBitVector {
  final def apply[F[_]: Concurrent](ch: Pipe[F, RESP, BitVector]): TestCasesLaserdiscRespBitVector[F] =
    new TestCasesLaserdiscRespBitVector[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscRespBitVector[F[_]: Concurrent](ch: Pipe[F, RESP, BitVector]) extends TestSendResp {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}
