package aoc2025

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day1a extends IOApp.Simple {

  case class State(st: (Int, Int))
  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine(scanner, state.copy(st = {
              val newPos = l.charAt(0) match {
                case 'L' => state.st._2 - (l.substring(1).toInt)
                case _ => state.st._2 + (l.substring(1).toInt)
              }
              val c: Int = if (newPos % 100 == 0) state.st._1 + 1 else state.st._1
              (c, newPos % 100)
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
