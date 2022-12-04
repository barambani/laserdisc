package laserdisc
package fs2

import laserdisc.fs2.parallel.SetUpLaserdiscCatsBaseline.LaserdiscCatsBaselineSetUp

final class ByteInByteOutBaselineBenchSpec extends Fs2BenchSuite(new LaserdiscCatsBaselineSetUp()) {
  test(s"porca madonna") {
    val result = run(longTestCase())
    assertEquals(result.size, 10)
  }
}
