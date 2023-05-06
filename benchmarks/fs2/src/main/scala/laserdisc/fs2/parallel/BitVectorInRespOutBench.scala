package laserdisc
package fs2
package parallel

import _root_.fs2
import cats.effect.IO
import laserdisc.auto._
import laserdisc.fs2.parallel.BitVectorRespBenchSetup.BitVectorRespSetUp
import laserdisc.fs2.parallel.channels.BitVectorInRespOutChannel
import laserdisc.fs2.parallel.runtime.BenchRuntime
import laserdisc.fs2.parallel.testcases.TestCasesBitVector
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.bits.BitVector

class BitVectorInRespOutBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def longCommandsLoad(setUp: BitVectorRespSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.longCommandsTest)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def shortCommandsLoad(setUp: BitVectorRespSetUp, bh: Blackhole): Unit = {
    val res = setUp.runWithLoad(setUp.testCases.shortCommandsTest)

    bh.consume(res)
  }
}

private object BitVectorRespBenchSetup {

  @State(Scope.Benchmark)
  class BitVectorRespSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    private var runtime: BenchRuntime = _

    private[fs2] val testCases = TestCasesBitVector()

    private val channel = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
      BitVectorInRespOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    @Setup(Level.Trial)
    def setup(): Unit =
      runtime = BenchRuntime()

    @TearDown
    def tearDown(): Unit =
      runtime.tearDown()

    private[fs2] def runWithLoad(load: Stream[fs2.Pure, BitVector]): Vector[RESP] =
      runtime.run(channel, load)
  }
}
