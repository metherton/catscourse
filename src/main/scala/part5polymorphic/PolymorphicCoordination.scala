package part5polymorphic

import cats.effect.kernel.{Spawn}
import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Outcome, Ref}
import utilsScala2.general.{DebugWrapper, unsafeSleep}

import scala.concurrent.duration._

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO]
  val aDeferred = Deferred[IO, Int] // given /implicict Concurrent[IO] in scope
  val aDeferred_v2 = concurrentIO.deferred[Int]
  val aRef = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError, uncancelabe, start (fibers), + ref/deferred

  import cats.syntax.flatMap._ // flatmap
  import cats.syntax.functor._ // map

  import cats.effect.syntax.spawn._

  def eggBoiler(): IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Egg boiling on some other fiber...").debug1
      _ <- signal.get // blocker
      _ <- IO(s"EGG READY").debug1
    } yield ()

    def tickingClock(ticks: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- IO(count).debug1
      _ <- if (count >= 10) signal.complete(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  def polymorphicEggBoiler[F[_]](implicit concurrent: Concurrent[F]): F[Unit] = {
    def eggReadyNotification(signal: Deferred[F, Unit]) = for {
      _ <- concurrent.pure("Egg boiling on some other fiber...").debug1
      _ <- signal.get // blocker
      _ <- concurrent.pure(s"EGG READY").debug1
    } yield ()

    def tickingClock(counter: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep[F, Throwable](1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- concurrent.pure(count).debug1
      _ <- if (count >= 10) signal.complete(()) else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- concurrent.ref(0)
      signal <- concurrent.deferred[Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  /*
      Exervcise
      1. Generalize racePair
      2. Generalize the mutex concurrency primitive for any F

   */

  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B]) // (loser fiber, winner result)
  ]
  // racing IOs lesson

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]
  import cats.effect.syntax.monadCancel._ // guaranteeCase extension method
  import cats.effect.syntax.spawn._ // start extension method
  def ourRacePair[F[_], A, B](fa: F[A], fb: F[B])(implicit concurrent: Concurrent[F]): F[RaceResult[F, A, B]] =
    concurrent.uncancelable { poll =>
    for {
      signal <- concurrent.deferred[EitherOutcome[F, A, B]]
      fiba <- fa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- fb.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelFibA <- fiba.cancel.start
          cancelFibB <- fibb.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      } // blocking call - should be cancelable
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibb))
      case Right(outcomeB) => Right((fiba, outcomeB))
    }
  }



  override def run: IO[Unit] = eggBoiler()
}
