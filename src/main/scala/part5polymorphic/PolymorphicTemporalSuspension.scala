package part5polymorphic

import cats.effect.{Concurrent, IO, IOApp}
import scala.concurrent.duration._

import java.time.temporal.Temporal
import scala.concurrent.duration.FiniteDuration
import utilsScala2.general._

object PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal - time blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this filter for a specified time

  }

  // abilities: pure, map / flatMap, raiseError, uncancelable, start, ref / deferred,  + sleep

  import cats.effect.Temporal

  val temporalIO = Temporal[IO] // given Temporal[IO] in scope
  val chainOfEffects = IO("Loading...").debug1 *> IO.sleep(1.second) *> IO("Game ready").debug1
  val chainOfEffects_V2 = temporalIO.pure("Loading...").debug1 *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready").debug1

  /*
      Exercise: generalize the following piece

   */

  import cats.syntax.flatMap._

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(implicit temporal: Temporal[F]): F[A] = {
    val timeoutEffect = temporal.sleep(duration)
    val result = temporal.race(fa, timeoutEffect)
    result.flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException("computation timeed out"))
    }
  }


  override def run: IO[Unit] = ???
}
