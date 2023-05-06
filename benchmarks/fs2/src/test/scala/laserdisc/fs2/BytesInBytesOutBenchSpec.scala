package laserdisc
package fs2

import laserdisc.fs2.parallel.BytesBenchSetUp.BytesSetUp
import laserdisc.fs2.parallel.BytesInBytesOutBench

final class BytesInBytesOutBenchSpec extends Fs2BenchSuite() {

  val runner = new BytesInBytesOutBench()

  var testSetup: BytesSetUp = _

  override def beforeEach(context: BeforeEach): Unit = {
    testSetup = new BytesSetUp()
    testSetup.setup()
  }
  override def afterEach(context: AfterEach): Unit =
    testSetup.tearDown()

  test(s"ByteInByteOutBaselineBenchSpec short commands terminates") {
    runMany(_ => runner.shortCommandsLoad(testSetup, blackhole))
    assert(cond = true, "this benchmark didn't complete")
  }

  test(s"ByteInByteOutBaselineBenchSpec long commands terminates") {
    runMany(_ => runner.longCommandsLoad(testSetup, blackhole))
    assert(cond = true, "this benchmark didn't complete")
  }
}
