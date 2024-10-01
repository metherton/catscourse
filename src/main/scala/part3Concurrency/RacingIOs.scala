package part3Concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import utils._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
   (
     IO(s"starting computation").debug1 >>
       IO.sleep(duration) >>
       IO(s"computation for $value: done") >>
       IO(value)
   ).onCancel(IO(s"computation CANCELED for $value").debug1.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    // if you want to get the loser val first: IO[Either[Int, String]] = unrace(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibres
      - the first one to finish will complete the result
      - the loser will be canceled
     */
    first.flatMap {
      case Left(mol) => IO(s"meaning of life won: $mol")
      case Right(lang) => IO(s"fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").debug1 >> IO(outMol).debug1
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Lang won").debug1 >> IO(outLang).debug1
    }
  }

  /** Exercises
   *  1 - implement a timeout pattern with race
   *  2 - implement a racing condition..method to return a LOSING effect from a race (hint: user racePair)
   *  3 - implement race in terms of racePair
   * */

  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect = IO.sleep(duration)
    val result = IO.race(io, timeoutEffect)
    result.flatMap {
      case Left(v) => IO(v)
      case Right(_) => IO.raiseError(new RuntimeException("computation timeed out"))
    }
  }

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Right(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser cancelled"))
      }
      case Right((fibA, _)) => fibA.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser cancelled"))
      }
    }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
        case Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled"))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled"))
        }
      }
    }


  private val importantTask: IO[Int] = IO.sleep(2.seconds) >> IO(42).debug1
  val testTimeout = timeout(importantTask, 1.seconds)
  val testTimeout_v2 = importantTask.timeout(1.seconds)

  override def run: IO[Unit] = testRace().debug1.void
}
