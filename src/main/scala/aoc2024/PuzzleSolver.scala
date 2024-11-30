package aoc2024

import aoc2024.PuzzleSolver.readLineByLine
import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.{getResourceFromFile, openFileScanner}
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object PuzzleSolver extends IOApp.Simple {

  case class State[S,+A](run: S => (A, S))

  def readLineByLine(scanner: Scanner): IO[Unit] = {
    def findNumber(s: String): String = {
      s.find(i => i.isDigit).head.toString + s.findLast(i => i.isDigit).head.toString
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
       // r = findNumber(l).toInt
        _ <- readLineByLine(scanner)
      } yield ()
    }
    else {
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def processInputFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner)
      }

  override def run: IO[Unit] = {
    // determine which puzzles are available to be solved
    // we can do this by getting all the files in the resources/aoc2024 directory
    val dir = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024")
    val inputs = dir.listFiles().toList.map(f => (f.getName.split("[.]")(0), f.getAbsoluteFile))

    import cats.syntax.parallel._
    import cats.instances.list._


    inputs.parTraverse(a => {
      resourceReadFile(a._2.getPath)
    }).void
  }

}
