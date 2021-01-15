package laserdisc
package fs2
package parallel
package channels

import _root_.fs2.netty.{Network, Socket}
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.flatMap._

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

object ByteInByteOutNettyChannel {

  private[fs2] final def apply[F[_]: ContextShift: Concurrent](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration]
  )(blocker: Blocker): Pipe[F, Byte, Byte] = {
    def connectedSocket: Resource[F, Socket[F]] =
      Network[F] >>= { net =>
        net.client(address, reuseAddress = false, keepAlive = true, noDelay = true)
      }

    stream =>
      Stream.resource(connectedSocket) >>= { socket =>
        stream
          .through(socket.writes).drain
          .mergeHaltBoth(socket.reads)
          .onFinalizeWeak(socket.close)
      }
  }
}
