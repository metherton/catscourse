package part2Effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential
  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug extenstion method
  import utils._
  import cats.syntax.apply._

  val meaningOfLife = IO.delay(42)
  val favLang = IO.delay("Scala")

  val goalInLife = (meaningOfLife.debug1, favLang.debug1).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IO
  // convert a sequential IO to parallel io
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug1)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug1)
  val goalInLifeParallel = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife_v2 = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand
  import cats.syntax.parallel._

  val goalInLife_v3: IO[String] = (meaningOfLife.debug1, favLang.debug1).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this"))

  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug1, aFailure.debug1).parMapN(_ + _)

  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))

  // the first effect to fail gives the failure of the result
  val twoFailures: IO[String] = (aFailure.debug1, anotherFailure.debug1).parMapN(_ + _)

  override def run: IO[Unit] =
    twoFailures.debug1.void
}
