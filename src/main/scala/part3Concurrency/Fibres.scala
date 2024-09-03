package part3Concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp}
import scala.concurrent.duration._

object Fibres extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import utils._

  def sameThreadIOs() = for {
    _ <- meaningOfLife.debug1
    _ <- favLang.debug1
  } yield ()

  // introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create Fibers manually


  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[FiberIO[Int]] = meaningOfLife.debug1.start

  def differentThreadIOs() =  for {
    _ <- aFiber
    _ <- favLang.debug1
  } yield ()

  // joining a Fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result

  /**
   * possible outcomes
   * - success with an IO
   * - failure with an exception
   * - cancelled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").debug1 >> IO.sleep(1.second) >> IO("done").debug1
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled").debug1.void)

    for {
      //fib <- task.start // on a separate thread
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("Cancelling").debug1 // running on the calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result

  }

  override def run: IO[Unit] = {
    //sameThreadIOs()
    //differentThreadIOs()
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug1.void
//    throwOnAnotherThread()
//      .debug1.void
    testCancel().debug1.void
  }
}
