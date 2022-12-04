package laserdisc
package fs2
package parallel

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.auto._
import laserdisc.fs2.parallel.SetUpLaserdiscCatsBaseline.LaserdiscCatsBaselineSetUp
import laserdisc.fs2.parallel.channels.ByteInByteOutChannel
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.runtime.asdsaasdasd
import laserdisc.fs2.parallel.testcases.TestCasesLaserdiscByteBaseline
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class LaserdiscCatsByteInByteOutBaselineBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsBaselineSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case1)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsBaselineSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case2)

    bh.consume(res)
  }
}

object SetUpLaserdiscCatsBaseline {

  @State(Scope.Benchmark)
  class LaserdiscCatsBaselineSetUp extends asdsaasdasd[Vector[Byte]] {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    var runtime: IORuntime = _
    private val channel = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
      ByteInByteOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    private[fs2] var testCases: TestCasesLaserdiscByteBaseline[IO] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      channel
        .map(ch => testCases = TestCasesLaserdiscByteBaseline(ch))
        .unsafeRunSync()(runtime)
    }

    @TearDown
    def tearDown(): Unit =
      runtime.shutdown()

    def run(run: IO[Vector[Byte]]): Vector[Byte] =
      run.unsafeRunSync()(runtime)

    def longTestCase(): IO[Vector[Byte]]  = testCases.case1
    def shortTestCase(): IO[Vector[Byte]] = testCases.case2
  }
}
