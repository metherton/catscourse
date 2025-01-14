package part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import utilsScala2.general.DebugWrapper


import scala.concurrent.duration._
object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }
  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("once started I can't go back")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._  // map

  def mustComputeGeneral[F[_], E](implicit mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("once started i can't go back")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being cancelled").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled").void) // same

  // .onCancel as extension method
  import cats.effect.syntax.monadCancel._ // .onCancel

  // allow finalizers: guarantee, guaranteeCase
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"successful: $a").void)
    case Errored(e) => IO(s"failed: $e").void
    case Canceled() => IO("canceled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage = monadCancelIO.bracket(IO(42)){
    value => IO(s"Using the meaning of life: $value")
  } {value =>
    IO("releasing the meaning of life...").void
  }

  /**
   * Exercise - generalize a piece of code
   *
   *
   * @return
   */

  // hint use this instead of IO.sleep
  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // not semantic blocking

  def inputPassword[F[_], E](implicit mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("input password").debug1
    _ <- mc.pure("typeing password").debug1
    _ <- unsafeSleep[F, E](5.seconds)
    pw <- mc.pure("RocktheJVM1!")
  } yield pw


  def verifyPassword[F[_], E](pw: String)(implicit mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("verifying...").debug1
    _ <- unsafeSleep[F, E](2.seconds)
    check <- mc.pure(pw == "RocktheJVM1!")
  } yield check

  def authFlow[F[_], E](implicit mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out. Try again later").debug1.void) // this is cancellable
      verified <- verifyPassword(pw) // This is NOT cancelable
      _ <- if (verified) mc.pure("Authentication succesful").debug1 // This is NOT cancelable
      else mc.pure("Authentication failed").debug1
    } yield ()
  }

  val authProgram: IO[Unit] = {
    for {
      authFib <- authFlow[IO, Throwable].start
      _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").debug1 >> authFib.cancel
      _ <- authFib.join
    } yield ()
  }

  override def run: IO[Unit] = authProgram
}
