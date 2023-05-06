package laserdisc
package fs2

import munit.FunSuite
import org.openjdk.jmh.infra.Blackhole

abstract class Fs2BenchSuite[A, B]() extends FunSuite {

  final val blackhole = new Blackhole(
    "Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."
  )

  def runMany(a: Int => Unit, iterations: Int = 45000): Unit =
    (1 to iterations).foreach { x =>
      try a(x)
      catch {
        case th: Throwable =>
          println("Failed after " + x + " iterations")
          throw th
      }
    }
}
