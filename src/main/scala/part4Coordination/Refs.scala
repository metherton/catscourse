package part4Coordination


import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import utils._
import scala.concurrent.duration._

object Refs extends IOApp.Simple {

  // ref = a purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying is an effect
  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43) // thread safe
  }

  // obtain a value
  val mol = atomicMol.flatMap { ref =>
    ref.get // thread safe
  }

  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  } // gets the old value and sets the new one

  // updating with a function
  val fMol = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updatedMol = atomicMol.flatMap {ref =>
    ref.updateAndGet(value => value * 10)  // get the new value
    // can also use getAndUpdate to get the OLD value
  }

  // modifying with a function returning a different type
  val modifiedMol: IO[String] = atomicMol.flatMap {ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  // why: concurrent + thread safe reads/writes over shared values, in a purely functional way

  import cats.syntax.parallel._


  def demoConcurrentWorkImpure(): IO[Unit] = {


    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug1
        newCount = count + wordCount
        _ <- IO(s"New total: $newCount").debug1
        _ <- IO(count += newCount)
      } yield ()
    }
    List("I love cats effect", "this ref thing is useless", "daniel writes a lot of code")
      .map(task)
      .parSequence
      .void

  }

  /*
      Drawbacks:
      - hard to read / debug
      - mix pure / impure code
      - NOT THREAD SAFE
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug1
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New total: $newCount").debug1
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love cats effect", "this ref thing is useless", "daniel writes a lot of code")
        .map(string => task(string, initialCount))
        .parSequence
    } yield ()
  }

  /*
      Exercise

   */


  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug1
      _ <- IO(ticks += 1) // not thread safe
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug1
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug1
      _ <- ticks.update(_ + 1) // thread safe effect
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"TICKS: $t").debug1
      _ <- printTicks(ticks)
    } yield ()

    for {
      tickRef <- Ref[IO].of(0)
      _ <- (tickingClock(tickRef), printTicks(tickRef)).parTupled
    } yield ()

  }

  def tickingClockWeird(): IO[Unit] = {
    val ticks = Ref[IO].of(0)
    def tickingClock: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug1
      _ <- t.update(_ + 1) // thread safe effect
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").debug1
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  override def run = tickingClockImpure()
}
