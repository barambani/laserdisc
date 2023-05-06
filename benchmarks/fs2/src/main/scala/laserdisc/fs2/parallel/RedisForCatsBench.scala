package laserdisc
package fs2
package parallel

import cats.effect.IO
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log
import laserdisc.fs2.parallel.SetUpRedisForCats.RedisForCatsSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime
import laserdisc.fs2.parallel.testcases.RedisForCatsTestCases
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class RedisForCatsBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case1)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case2)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case3)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case4)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case5)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val res = setUp.run(setUp.testCases.case6)

    bh.consume(res)
  }
}

private object SetUpRedisForCats {

  @State(Scope.Benchmark)
  class RedisForCatsSetUp {
    import Log.Stdout._

    private var runtime: BenchRuntime = _

    private val resource = Redis[IO].utf8("redis://localhost")

    private[fs2] var testCases: RedisForCatsTestCases[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]              = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = BenchRuntime()
      runtime.runUnsafeSync(
        resource.allocated
          .map { case (rc, cu) =>
            testCases = RedisForCatsTestCases(rc)
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
