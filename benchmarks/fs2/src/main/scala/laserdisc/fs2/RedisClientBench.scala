package laserdisc.fs2

import cats.effect.{IO, Resource}
import cats.syntax.traverse._
import laserdisc._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.SetUpLaserdiscCatsRespByte.LaserdiscState
import laserdisc.fs2.parallel.runtime.BenchRuntime
import org.apache.commons.pool2.impl.GenericObjectPoolConfig
import org.openjdk.jmh.annotations._
import redis.clients.jedis.{Jedis, JedisPool}

private object SetUpLaserdiscCatsRespByte {

  @State(Scope.Benchmark)
  class LaserdiscState {
    var runtime: BenchRuntime = _

    var client: RedisClient[IO]         = _
    private var clientCleanUp: IO[Unit] = _

    @Setup
    def setup(): Unit = {
      runtime = BenchRuntime.fixedThreadRuntime
      runtime.runUnsafeSync(
        RedisClient[IO]
          .to("localhost", 6379)
          .allocated
          .map { case (rc, cu) =>
            client = rc
            clientCleanUp = cu
          }
      )
    }

    @TearDown
    def tearDown(): Unit = {
      runtime.runUnsafeSync(clientCleanUp)
      runtime.tearDown()
    }
  }
}

@State(Scope.Benchmark)
class JedisState {
  var jedisPool: JedisPool = _

  var runtime: BenchRuntime = _

  @Setup
  def setup(): Unit = {
    val poolConfig = new GenericObjectPoolConfig
    poolConfig.setMaxTotal(8)
    poolConfig.setMinIdle(0)
    poolConfig.setMaxIdle(0)
    jedisPool = new JedisPool(poolConfig, "localhost", 6379)
    runtime = BenchRuntime.fixedThreadRuntime
  }

  @TearDown
  def tearDown(): Unit = {
    jedisPool.close()
    runtime.tearDown()
  }
}

class RedisClientBench {
  import RedisClientBench._

  def writeAndReadForKey[K](key: K, n: Int, write: (K, Int) => IO[Any], read: K => IO[Any]): IO[Int] = {
    val values = List.fill(n)((key, n))
    values
      .traverse { case (key, value) =>
        write(key, value) *> read(key)
      }
      .map(_.size)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def jedisWriteAccum100(jedisState: JedisState): Int =
    jedisState.runtime.runUnsafeSync(
      Resource
        .fromAutoCloseable[IO, Jedis](IO(jedisState.jedisPool.getResource))
        .use { jedisClient =>
          writeAndReadForKey[String](
            s"test-key:$numberOfItems",
            numberOfItems,
            (k, v) => IO(jedisClient.set(k, v.toString)),
            k => IO(jedisClient.get(k))
          )
        }
    )

  @Benchmark
  @OperationsPerInvocation(100)
  def laserdiscWriteAccum100(laserdiscState: LaserdiscState): Int = {
    val client = laserdiscState.client
    val key    = Key.unsafeFrom(s"test-key:$numberOfItems")
    laserdiscState.runtime.runUnsafeSync(
      writeAndReadForKey[Key](
        key,
        numberOfItems,
        (k, v) => client.send(set(k, v)),
        k => client.send(get(k))
      )
    )
  }
}

object RedisClientBench {
  val numberOfItems = 100
}
