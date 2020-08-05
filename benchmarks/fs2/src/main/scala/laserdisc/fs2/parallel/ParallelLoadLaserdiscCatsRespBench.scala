package laserdisc
package fs2
package parallel

import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{Blocker, ContextShift, IO, Timer}
import cats.syntax.flatMap._
import laserdisc.auto._
import laserdisc.fs2.parallel.SetUpLaserdiscCatsResp.LaserdiscCatsRespSetUp
import laserdisc.fs2.parallel.testcases.TestCasesLaserdiscResp
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor
import scala.concurrent.duration._

class ParallelLoadLaserdiscCatsRespBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsRespSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsRespSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()

    bh.consume(res)
  }
}

object SetUpLaserdiscCatsResp {

  @State(Scope.Benchmark)
  class LaserdiscCatsRespSetUp {
    private[this] val commandsService           = Executors.newFixedThreadPool(8)
    private[this] val ec: ExecutionContext      = fromExecutor(commandsService)
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
    implicit val timer: Timer[IO]               = IO.timer(ec)
    implicit val logWriter: LogWriter[IO]       = SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    val resource = Blocker[IO] evalMap { bl =>
      RedisAddress("localhost", 6379).toInetSocketAddress[IO] map { address =>
        RedisChannel[IO](address, writeTimeout = Some(10.seconds), readMaxBytes = 8 * 1024 * 1024)(bl)
      }
    }

    private[fs2] var testCases: TestCasesLaserdiscResp[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]               = _

    @Setup(Level.Trial)
    def setup(): Unit =
      resource.allocated
        .map {
          case (rc, cu) =>
            testCases = TestCasesLaserdiscResp(rc)
            clientCleanUp = cu
        }
        .unsafeRunSync()

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      (clientCleanUp >>
        IO.delay(commandsService.shutdown()) >>
        IO.delay(commandsService.awaitTermination(2, TimeUnit.SECONDS)) >>
        IO.unit).unsafeRunSync()
  }
}
