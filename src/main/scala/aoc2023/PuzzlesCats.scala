package aoc2023

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.openFileScanner

import java.util.Scanner
import utils._
import scala.concurrent.duration._

object PuzzlesCats extends IOApp.Simple {

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug1 >> IO.sleep(100.millis) *> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  override def run: IO[Unit] = bracketReadFile("src/main/resources/5.txt").void
}
