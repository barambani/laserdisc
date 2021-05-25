package laserdisc
package fs2

import cats.effect.IO
import cats.syntax.either._
import cats.syntax.flatMap._
import laserdisc.auto._
import laserdisc.fs2.parallel.runtime.BenchRuntime.fixedFixedRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdisc

import scala.concurrent.duration.DurationInt

object CatsIoTestRunner {

//  private[this] implicit val logWriter: LogWriter[IO] = consoleLogUpToLevel(LogLevels.Error)

  def main(args: Array[String]): Unit = {

    val runFor = 15.minutes

    val laserdiscClientTask = IO.monotonic >>= { start =>
      RedisClient[IO].to("localhost", 6379).use { cl =>
        val cases = TestCasesLaserdisc[IO](cl)
        0L.tailRecM[IO, Long] { count =>
          (cases.case1 >> IO.monotonic).map { current =>
            if (current - start >= runFor) count.asRight
            else (count + 1L).asLeft
          }
        }
      }
    }

//    val bitVectorInTask = IO.monotonic >>= { start =>
//      val channel = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
//        BitVectorInByteOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
//      }
//      val cases = channel.flatMap(ch => TestCasesLaserdiscBitVectorByte[IO](ch).case1)
//      0L.tailRecM[IO, Long] { count =>
//        (cases >> IO.monotonic).map { current =>
//          if (current - start >= runFor) count.asRight
//          else (count + 1L).asLeft
//        }
//      }
//    }

    println(s"Avg send/s: ${laserdiscClientTask.unsafeRunSync()(fixedFixedRuntime) * 48d / runFor.toMinutes / 60}")
    sys.exit()
  }
}
