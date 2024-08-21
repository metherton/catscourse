package part2Effects

import cats.effect.IO
import cats.effect.IO.delay

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  // IO - pure, delay, defer
  // create failed effects
  val aFailedCompute:IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))

  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail")) // use this in preference to the previous one

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _ : RuntimeException => IO.delay(println("I'm still here"))
      // add more cases here
  }

  // turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  // redeem: transforms the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL $ex", value => s"SUCCESS $value" )

  // redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"Fail $ex")), value => IO(println(s"SUCCESS: $value")))

  /**
   * Exercises
   *
   */
    // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case None => IO.raiseError(ifEmpty)
    case Some(v) => IO.pure(v)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Success(value) => IO.pure(value)
    case Failure(exception) => IO.raiseError(exception)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(e) => IO.raiseError(e)
    case Right(v) => IO.pure(v)
  }

  // 2 - handleError & handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    val bla: Unit = resultAsEffect.unsafeRunSync()
  }

}
