package aoc2023

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.openFileScanner

import java.util.Scanner
import utils._
import scala.concurrent.duration._

object PuzzlesCats extends IOApp.Simple {

  def readLineByLine(scanner: Scanner, acc: Int): IO[Unit] = {
    def findNumber(s: String): String = {
      s.find(i => i.isDigit).head.toString + s.findLast(i => i.isDigit).head.toString
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        r = findNumber(l).toInt
        _ <- readLineByLine(scanner, acc + r)
      } yield ()
    }
    else {
      IO(s"final total is $acc").debug1 *> IO.unit
    }
  }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner, 0)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  override def run: IO[Unit] = bracketReadFile("src/main/resources/1.txt").void
}
