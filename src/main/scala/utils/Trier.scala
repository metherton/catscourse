package utils

import scala.util.Try

object Trier extends App {

  val firstTry = Try {42}
  val secondTry = Try {43}
  val result = for {
    f <- firstTry
    s <- secondTry
  } yield (f, s)
  println(result)
}

object EitherApp extends App {

  val firstEither: Either[Throwable, Int] = Left {throw new RuntimeException("unable to calculat")}
  val secondEither: Either[Throwable, Int] = Right {43}
  val result = (for {
    f <- firstEither
    s <- secondEither
  } yield (f, s)) match {
    case Right((x, y)) => {
      println(s"$x + $y")
      x + y
    }
    case Left(e) => {
      println(s"Error: ${e.getMessage}")
      e
    }
  }
  println(result)
}
