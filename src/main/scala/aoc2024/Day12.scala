package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper
import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day12  extends IOApp.Simple {

  case class Coords(row: Int, col: Int)
  case class Plot(id: Char, coords: Coords)
  case class Region(plots: List[Plot])
  case class Grid(plots: List[Plot])
  case class State(grid: Grid)
  def readLineByLine(scanner: Scanner, state: State, rowNo: Int): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        plots = l.toCharArray.toList.zipWithIndex.map(p => Plot(p._1, Coords(rowNo, p._2)))
        _ <- readLineByLine(scanner, state.copy(grid = Grid(plots ::: state.grid.plots)), rowNo + 1)
      } yield ()
    }
    else {
      val result = state.grid
      IO(s"final states is ${result}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(Grid(List())), 0)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/12.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
