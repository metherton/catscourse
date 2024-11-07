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

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen..but evaluation is lazy in this case as it uses a by name call..check difference in implementation


  def sequenceTakeLast_v3a[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

//  def sequenceTakeLastV3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
//    ioa >> iob // "andThen" with by-name call

 // 2 - sequence two IOs and take the result of the FIRST one
 // hint: use flatMap
  //
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob


  // 3 - Repeat an IO effect forever
 // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def foreverV2[A](io: IO[A]): IO[A] =
    io >> foreverV2(io) // recommend using the lazy andThen instead of the eager one because it prevents stack overflow with tail recursice algorigthm

  def foreverV3[A](io: IO[A]): IO[A] =
    io *> foreverV3(io)

  def foreverV4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

  // 4 - convert an IO to a different type
  // hint - use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convertV2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnitV2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // discourage - don't use this

  def asUnitV3[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield prevSum + lastNumber


  // 7 - (hard) - write a fibanacci IO that does not crash on recursion
  // hints - use recursion, ignore exponential complexity, use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1))   // same as .delay(...).flatten
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev


  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform"
    // "end of the world"
    val first: IO[Int] = IO.pure(42)
    val last: IO[Int] = IO.pure(43)
 //   println(sequenceTakeLast(first, last).unsafeRunSync())
 //   println(sequenceTakeFirst(first, last).unsafeRunSync())
 //   println(sumIO(20000).unsafeRunSync())
 //   (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))
//    foreverV4(IO{
//      println("forever!")
//      Thread.sleep(100)
//    }).unsafeRunSync()
  }
}
