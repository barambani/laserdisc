package laserdisc
package fs2

import _root_.fs2._
import _root_.fs2.io.net.{Network, Socket, SocketOption}
import cats.MonadError
import cats.effect.{Concurrent, Resource}
import cats.syntax.flatMap._
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.protocol.resp._
import log.effect.fs2.LogSelector

object RedisChannel {

  private[fs2] final def apply[F[_]: Network: LogSelector: Concurrent](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Pipe[F, RESP, RESP] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes)) >>= { socket =>
        val send    = stream.through(impl.send(socket.write))
        val receive = socket.reads.through(impl.receiveResp)

        send.drain
          .covaryOutput[RESP]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }

  private[fs2] final def connectedSocket[F[_]: Network](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Resource[F, Socket[F]] =
    Network[F].client(
      address,
      List(
        SocketOption.noDelay(true),
        SocketOption.receiveBufferSize(receiveBufferSizeBytes)
      )
    )

  private[this] final object impl {
    def send[F[_]: LogSelector: MonadError[*[_], Throwable]](socketWrite: Chunk[Byte] => F[Unit])(
        implicit logSelector: LogSelector[F]
    ): Pipe[F, RESP, Unit] =
      _.evalTap(resp => logSelector.log.trace(s"sending $resp"))
        .map(RESP.respCodec.encode)
        .evalMap(arr => socketWrite(Chunk.array(arr)))

    def receiveResp[F[_]](implicit logSelector: LogSelector[F], monadError: MonadError[F, Throwable]): Pipe[F, Byte, RESP] = {
      def framing: Pipe[F, Byte, CompleteFrame] = {
        def loopScan(bytesIn: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
          bytesIn.pull.uncons.flatMap {
            case Some((chunk, rest)) =>
              previous.append(chunk.toArray) match {
                case Left(ex)                    => Pull.raiseError(ex)
                case Right(frame: CompleteFrame) => Pull.output1(frame) >> loopScan(rest, EmptyFrame)
                case Right(frame: MoreThanOneFrame) =>
                  Pull.output(Chunk.vector(frame.complete)) >> {
                    if (frame.remainder.isEmpty) loopScan(rest, EmptyFrame)
                    else loopScan(rest, IncompleteFrame(frame.remainder, 0L))
                  }
                case Right(frame: IncompleteFrame) => loopScan(rest, frame)
              }

            case _ => Pull.done
          }

        bytesIn => loopScan(bytesIn, EmptyFrame).stream
      }

      pipeIn =>
        pipeIn
          .through(framing)
          .evalMap(frame =>
            RESP.respCodec
              .decode(frame.bytes)
              .fold[F[RESP]](
                err => monadError.raiseError(new Exception(err.m)),
                res => monadError.pure(res.step)
              )
          )
          .evalTap(resp => logSelector.log.trace(s"receiving $resp"))
    }
  }
}
