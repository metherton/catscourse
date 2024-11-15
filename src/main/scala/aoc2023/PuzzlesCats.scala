package aoc2023

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.openFileScanner

import java.util.Scanner
import utils._
import scala.concurrent.duration._

object PuzzlesCats extends IOApp.Simple {

  import cats.syntax.functor._
  import cats.syntax.flatMap._
  var sum: Int = 0
  def readLineByLine(scanner: Scanner): IO[Unit] = {
    def findNumber(s: String): String = {
      s.find(i => i.isDigit).head.toString + s.findLast(i => i.isDigit).head.toString
    }
    if (scanner.hasNextLine) {
      //IO(scanner.nextLine()).debug1 >> IO.sleep(100.millis) *> readLineByLine(scanner)
      val res = (for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(100.millis)
        r = findNumber(l)
        _ <- readLineByLine(scanner)
      } yield ())
      res
    }
    else IO.unit
  }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  override def run: IO[Unit] = bracketReadFile("src/main/resources/1.txt").void
}
