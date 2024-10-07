package part3Concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration._
import utils._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).debug1 // SEMANTIC blocking
    _ <- IO.sleep(1.second).debug1
  } yield ()

  // really Blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  // yielding
  val iosOnManyThreads = for {
    _ <- IO("first").debug1
    _ <- IO.cede // a signal to yield control over the thread - equivalent to IO.shift
    _ <- IO("second").debug1
    _ <- IO.cede
    _ <- IO("third").debug1
  } yield ()

  def testThousandEffectsSwitch() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug1 >> IO.cede >> _.debug1).evalOn(ec)
  }

  /*
      blocking calls & IO.sleep and yield control over the calling thread automatically
   */
  override def run = testThousandEffectsSwitch().void
}
