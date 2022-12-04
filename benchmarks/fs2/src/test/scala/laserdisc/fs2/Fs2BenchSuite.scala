package laserdisc
package fs2

import cats.effect.IO
import laserdisc.fs2.parallel.runtime.asdsaasdasd
import munit.FunSuite

abstract class Fs2BenchSuite[A](asdsaasdasd: asdsaasdasd[A]) extends FunSuite {
  override final def beforeAll(): Unit =
    asdsaasdasd.setup()

  override final def afterAll(): Unit =
    asdsaasdasd.tearDown()

  def run(run: IO[A]) =
    asdsaasdasd.run(run)

  def longTestCase(): IO[A] =
    asdsaasdasd.longTestCase()

  def shortTestCase(): IO[A] =
    asdsaasdasd.shortTestCase()
}
