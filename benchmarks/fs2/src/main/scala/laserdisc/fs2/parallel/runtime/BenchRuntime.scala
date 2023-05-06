package laserdisc
package fs2
package parallel
package runtime

import _root_.fs2
import cats.effect.IO
import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final case class BenchRuntime(private val runtime: IORuntime) {
  def run[A, B](channel: IO[fs2.Pipe[IO, A, B]], testCase: fs2.Stream[fs2.Pure, A]): Vector[B] =
    channel.flatMap(ch => testCase.through(ch).compile.toVector).unsafeRunSync()(runtime)

  def runUnsafeSync[A](task: IO[A]): A =
    task.unsafeRunSync()(runtime)

  val computeEc: ExecutionContext = runtime.compute

  def tearDown(): Unit =
    runtime.shutdown()
}

object BenchRuntime {

  private[fs2] final def apply(): BenchRuntime = {

    def namedThreadFactory(name: String) = new ThreadFactory {
      val count = new AtomicInteger(0)
      override def newThread(r: Runnable): Thread =
        new Thread(r, s"$name-${count.incrementAndGet()}")
    }

    val computeExecutor  = Executors.newFixedThreadPool(8, namedThreadFactory("bench-compute-pool"))
    val blockingExecutor = Executors.newFixedThreadPool(4, namedThreadFactory("bench-blocking-pool"))

    val scheduler = Scheduler.fromScheduledExecutor(Executors.newSingleThreadScheduledExecutor(namedThreadFactory("bench-scheduler")))

    BenchRuntime(
      IORuntime(
        compute = fromExecutor(computeExecutor),
        blocking = fromExecutor(blockingExecutor),
        scheduler = scheduler,
        shutdown = () => {
          computeExecutor.shutdown()
          blockingExecutor.shutdown()
          val _ = computeExecutor.awaitTermination(2, TimeUnit.SECONDS)
          val _ = blockingExecutor.awaitTermination(2, TimeUnit.SECONDS)
        },
        config = IORuntimeConfig()
      )
    )
  }

  private[fs2] val fixedThreadRuntime = BenchRuntime()
}
