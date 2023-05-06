package laserdisc
package fs2

import cats.effect._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.LaserdiscFs2ClientSequentialSpec.SeqClientSpecIo

final class RedisClientSeqSpec extends SeqClientSpecIo(6379, "redis")
final class KeyDbClientSeqSpec extends SeqClientSpecIo(6380, "keyDb")

object LaserdiscFs2ClientSequentialSpec {
  abstract class SeqClientSpecIo(p: Port, dest: String) extends LaserdiscFs2ClientSequentialSpec(p, dest)
}

sealed abstract class LaserdiscFs2ClientSequentialSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def runManyAndForget[A](ioa: IO[Maybe[A]], iterations: Int): Unit =
    (1 to iterations).foreach { x =>
      ioa
        .handleError(th => fail("Failed after " + x + " iterations", th))
        .unsafeRunSync()(runtime)
    }

  private[this] final val maxSequentialIterations = 50 * 1000

  test(s"an fs2 $dest client handles correctly long sequences of short REST commands") {
    runManyAndForget(client.send(set("a key", 17)), iterations = maxSequentialIterations)
    assert(cond = true, "this benchmark didn't complete")
  }
}
