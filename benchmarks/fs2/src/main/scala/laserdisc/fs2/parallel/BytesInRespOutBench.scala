package laserdisc
package fs2
package parallel

import _root_.fs2
import cats.effect.IO
import laserdisc.auto._
import laserdisc.fs2.parallel.BytesRespBenchSetup.BytesRespSetUp
import laserdisc.fs2.parallel.channels.ByteInRespOutChannel
import laserdisc.fs2.parallel.runtime.BenchRuntime
import laserdisc.fs2.parallel.testcases.TestCasesBytes
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class BytesInRespOutBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def longCommandsLoad(setUp: BytesRespSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.longCommandsTest)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def shortCommandsLoad(setUp: BytesRespSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.shortCommandsTest)

    bh.consume(res)
  }
}

private object BytesRespBenchSetup {
  @State(Scope.Benchmark)
  final class BytesRespSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    var runtime: BenchRuntime = _

    private[fs2] val testCases = TestCasesBytes()

    private val channel = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
      ByteInRespOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    @Setup(Level.Trial)
    def setup(): Unit =
      runtime = BenchRuntime.fixedThreadRuntime

    @TearDown
    def tearDown(): Unit =
      runtime.tearDown()

    private[fs2] def runWithLoad(load: Stream[fs2.Pure, Byte]): Vector[RESP] =
      runtime.run(channel, load)
  }
}
