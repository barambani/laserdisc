package laserdisc
package fs2
package parallel
package runtime

import cats.effect.IO

trait asdsaasdasd[A] {
  def setup(): Unit
  def tearDown(): Unit
  def run(run: IO[A]): A
  def longTestCase(): IO[A]
  def shortTestCase(): IO[A]
}
