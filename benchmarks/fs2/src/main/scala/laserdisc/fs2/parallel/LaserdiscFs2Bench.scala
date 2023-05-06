package laserdisc
package fs2
package parallel

import cats.effect.IO
import laserdisc.auto._
import laserdisc.fs2.parallel.LaserdiscFs2BenchSetup.LaserdiscCatsSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdisc
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class LaserdiscFs2Bench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case1)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case2)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case3)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case4)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case5)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case6)

    bh.consume(res)
  }
}

private object LaserdiscFs2BenchSetup {

  @State(Scope.Benchmark)
  class LaserdiscCatsSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    private var runtime: BenchRuntime = _

    private val client = RedisClient[IO].to("localhost", 6379)

    private[fs2] var testCases: TestCasesLaserdisc[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]           = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = BenchRuntime()
      runtime.runUnsafeSync(
        client.allocated
          .map { case (rc, cu) =>
            testCases = TestCasesLaserdisc(rc)
            clientCleanUp = cu
          }
      )
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      runtime.runUnsafeSync(clientCleanUp)
      runtime.tearDown()
    }

    private[fs2] def run[A](load: IO[A]): A =
      runtime.runUnsafeSync(load)
  }
}
