package part3Concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import utils._

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on the some other thread..")
    42
  }
  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit = {
    threadPool.execute(() => computeMeaningOfLife())
  }

  // lift computation to an IO
  // async is an FFI - foreign function interface
  val asyncMolIO: IO[Int] = IO.async_ { (cb: Callback[Int]) => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaningOfLifeEither()
      cb(result) // CE thread is notified with the result
    }
  }

  /**
   * Exercise: lift an async computation on ec to an IO
   * @return
   */
    def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
      IO.async_[A] { (cb: Callback[A]) =>
        ec.execute { () =>
          val result = Try(computation()).toEither
          cb(result)
        }
      }
    }

    val asyncMolIO_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  /**
   * Exercise: lift an async computation as a Future to an IO
   * @return
   */
  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { cb: Callback[A] =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }
  lazy val molFuture = Future { computeMeaningOfLife() }
  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  /**
   * Exercise: can you define a never ending IO
   * @return
   */

  val neverEndingIO: IO[Int] = IO.async_[Int](_ => ())
  val neveEndingIO_v2: IO[Int] = IO.never

  import scala.concurrent.duration._

  /**
   *  FULL ASYNC call
   * @return
   */

  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      /*
          finalizer in case our computation gets cancelled.
          finalizers are of type IO[Unit]
          not specifying finalizer => Option[IO[Unit]]
          creating option is an effect => IO[Option[IO[Unit]]]

       */
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled").debug1.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("cancelling....").debug1 >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = demoAsyncCancellation.debug1 >> IO(threadPool.shutdown())
}
