package part3Concurrency


import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome}

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


  /**
   * Exercises
   * 1, Write a function that runs an IO on another thread, and, depending on the result of the fiber when you join it will
   * - return the result in an IO
   * - if errored or cancelled return a failed IO
   *
   * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results
   * - if both IOs complete successfully , tuple their results
   * - if the first IO returns an error , raise that error (ignoring the second IOS  result / error)
   * - if the first doesn't error but the second does raise that error
   *  - if one (or both) canceled raise a RuntimeException
   *
   *  3. Write a function that adds a timeout to an IO:
   *  - IO runs on a fiber
   *  - if the timeout duration passes, then the fiber is canceled
   *  - the method returns an IO[A] which contains
   *    - the original value if the computation is successful before the timeout signal
   *    - the exception if the computation is failed before the timeout signal
   *    - a RuntimeException if it times out (i.e cancelled by the timeout)
   */

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful - fibers can leak
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("computation canceled"))
    }
  }

  // 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fib <- io.debug1.start
      result <- fib.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("operation cancelled"))
    }
  }


  def testEx1() = {
    val aComputation = IO("Starting").debug1 >> IO.sleep(1.second) >> IO("done !").debug1 >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug1
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug1
    tupleIOs(firstIO, secondIO).debug1.void
  }

  def testEx3()    = {
    val aComputation = IO("Starting").debug1 >> IO.sleep(1.second) >> IO("done !").debug1 >> IO(42)
    timeout(aComputation, 500.millis).debug1.void
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val ioResult = for {
      fib1 <- ioa.start
      fib2 <- iob.start
      result1 <- fib1.join
      result2 <- fib2.join
    } yield (result1, result2)
    ioResult.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e),_) => IO.raiseError(e)
      case (_,Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("operation cancelled"))
    }
  }


  // 3


  override def run: IO[Unit] = {
    //sameThreadIOs()
    //differentThreadIOs()
    //runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug1.void
//    throwOnAnotherThread()
//      .debug1.void
    // testCancel().debug1.void
    //testEx1()
    //testEx2()
    testEx3()

  }
}
