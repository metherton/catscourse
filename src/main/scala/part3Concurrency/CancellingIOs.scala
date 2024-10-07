package part3Concurrency

import cats.effect.{IO, IOApp}

import utils._
import scala.concurrent.duration._
object CancellingIOs extends IOApp.Simple {

  /*
      Cancelling IOs
      - fib.cancel
      - IO.race & other APIs
      - manual cancellation
   */


  val chainOfIOs: IO[Int] = IO("waiting").debug1 >> IO.canceled >> IO(42).debug1 // output will actually be cancelled

  // uncancellable
  // example: online store, payment processor
  // payment process MUST NOT be cancelled
  val specialPaymentSystem = (
    IO("Payment running , don't cancel me...").debug1 >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debug1
  ).onCancel(IO("MEGA CANCEL OF DOOM").debug1.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // masking

  val atomicPayment_v2 = specialPaymentSystem.uncancelable

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation..").debug1 >> fib.cancel
    _ <- fib.join
  } yield ()


  /*
      The uncancelable API is more complex and general
      It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
      The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED
   */

  /*

      Example: authentication service. Has 2 parts
      - input password, can be cancelled, because otherwise we might block indefinitely on user input
      - verify password, CANNOT be cancelled once its started
   */

  val inputPassword = IO("input password").debug1 >> IO("typeing password").debug1 >> IO.sleep(2.seconds) >> IO("RocktheJVM1!")
  val verifyPassword = (pw: String) => IO("verifying...").debug1 >> IO.sleep(2.seconds) >> IO(pw == "RocktheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later").debug1.void) // this is cancellable
      verified <- verifyPassword(pw) // This is NOT cancelable
      _ <- if (verified) IO("Authentication succesful").debug1 // This is NOT cancelable
      else IO("Authentication failed").debug1
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").debug1 >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
      Uncancelable calls are MASKS which suppress cancellation
      Poll calls are "gaps opened" in the uncancelable region
   */


  /*

      Exercises

   */
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug1
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug1)
  // uncancelable will eliminate all Cancel points


  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").debug1 >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug1 >> IO.sleep(1.second)) >> IO("Cancelable end").debug1 >>
        IO("uncancelable").debug1 >> IO.sleep(1.second) >> IO("uncancelable end").debug1 >>
        poll(IO("second uncancelable").debug1 >> IO.sleep(1.second) >> IO("second cancelable end").debug1)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("Cancelling").debug1 >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = threeStepProgram()
}
