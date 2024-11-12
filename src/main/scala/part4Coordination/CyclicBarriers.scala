package part4Coordination

import cats.effect.kernel.Ref
import cats.effect.std.CyclicBarrier
import cats.effect.{Deferred, IO, IOApp}
import cats.syntax.parallel._
import utils._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object CyclicBarriers extends IOApp.Simple {

  /*
      A cyclic barrier is a coordination primitive that
      - is initialized with a count
      - has a single API: await

      A cyclic barrier will (semantically) block all fibers calling its await() method until we have exactly N fibers waiting
      at which point the barrier will unblock all fibers and reset to its original state

      Any further fiberwill again block until we have exactly N fibers waiting
      ...
      And so on

   */

  // example : Signing up for a social network just about to be launched
  //def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] = for {
  def createUser(id: Int, barrier: CBarrier): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
    _ <- IO(s"[user $id] Just heard theres a new social network - signing up for the waitlist").debug1
    _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
    _ <- IO(s"[user $id] On the waitlist now, can't wait").debug1
    _ <- barrier.await // block the fiber when there are exactly N users waiting
    _ <- IO(s"[user $id] OMG this is so cool").debug1
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO(s"[announcer] THe rock the jvm social network is up for registatatoin, launcing when we have 10 users").debug1
    //barrier <- CyclicBarrier[IO](10)
    barrier <- CBarrier(10)
    _ <- (1 to 20).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  /*
      Exercise Implement your own CB with Ref + Deferred


   */

  override def run: IO[Unit] = openNetwork()
}

abstract class CBarrier {
  def await: IO[Unit]
}

object CBarrier {

  case class State(nWaiting: Int, signal: Deferred[IO, Unit])
  def apply(count: Int): IO[CBarrier] = for {
    signal <- Deferred[IO, Unit]
    state <- Ref[IO].of(State(count, signal))
  } yield new CBarrier {
    override def await: IO[Unit] = Deferred[IO, Unit].flatMap { newSignal =>
      state.modify {
        case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
        case State(n, signal) => State(n - 1, signal) -> signal.get
      }.flatten
    }
  }
}