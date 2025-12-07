package aoc2025

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._
import scala.math.abs

object Day1b extends IOApp.Simple {

  type Counter = Int
  type CurrentPosition = Int


  case class State(st: (Int, Int))

  case class WrapResult(newPosition: Int, passes: Int)

  def wrapWithPasses(position: Int, delta: Int): WrapResult = {
    (position, delta) match {
      /*

             begin   delta   number of times zero
             5       10      0
             95      10      1
             95      5       1

       */


      // original position positive and delta positive - ok
      case (p, d) => {
        // we add an extra one if we went from a positive to negative threshold
        WrapResult((p + d) % 100, abs((p / 100) - (p + d) / 100) + (if ((p) * (p + d) < 0) 1 else 0))
      }
    }
  }
  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine(scanner, state.copy(st = {
              val newWrapWithPasses = l.charAt(0) match {
                case 'L' => wrapWithPasses(state.st._2, -(l.substring(1).toInt))
                case _ => wrapWithPasses(state.st._2,(l.substring(1).toInt))
              }
              val newCount: Int = (newWrapWithPasses.newPosition, newWrapWithPasses.passes) match {
                case (0, x) => (if (x > 0) (x - 1) else 0) + state.st._1 + 1
                case (_, x) => x + state.st._1
              }
              (newCount, newWrapWithPasses.newPosition)
            }))
      } yield ()
    }
    else {
      IO(s"final total is ...${state.st._1}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State((0,50)))
      }
  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2025/1.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
