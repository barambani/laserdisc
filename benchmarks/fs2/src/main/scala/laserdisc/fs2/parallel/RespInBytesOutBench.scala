package laserdisc
package fs2
package parallel

import _root_.fs2
import cats.effect.IO
import laserdisc.auto._
import laserdisc.fs2.parallel.RespByteBenchSetup.RespByteSetUp
import laserdisc.fs2.parallel.channels.RespInByteOutChannel
import laserdisc.fs2.parallel.runtime.BenchRuntime
import laserdisc.fs2.parallel.testcases.TestCasesResp
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class RespInBytesOutBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def longCommandsLoad(setUp: RespByteSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.longCommandsTest)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def shortCommandsLoad(setUp: RespByteSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.shortCommandsTest)

    bh.consume(res)
  }
}

private object RespByteBenchSetup {

  @State(Scope.Benchmark)
  class RespByteSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    private var runtime: BenchRuntime = _

    private[fs2] val testCases = TestCasesResp()

    private val channel = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
      RespInByteOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    @Setup(Level.Trial)
    def setup(): Unit =
      runtime = BenchRuntime()

    @TearDown
    def tearDown(): Unit =
      runtime.tearDown()

    private[fs2] def runWithLoad(load: Stream[fs2.Pure, RESP]): Vector[Byte] =
      runtime.run(channel, load)
  }
}
