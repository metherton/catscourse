package part2Effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay({ // use delay if you are not sure if the expression produces side effects
    println("I'm producing an integer")
    54
  })

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I'm producing an apply integer")
    43
  }

  val shouldNotDoThis = IO.pure{
    println("I'm really producing an integer")
    54
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(mol))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /*
      Exercises

   */


  def main(args: Array[String]): Unit = {
    // import cats.effect.unsafe.implicits.global // "platform"
    // "end of the world"
    println(smallProgram.unsafeRunSync())
  }
}
