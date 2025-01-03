package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day10  extends IOApp.Simple {

  case class Coords(row: Int, col: Int)
  case class Point(id: Int, coords: Coords)
  case class Grid(points: List[Point]) {
    def left(p: Point): Point = {
      p
    }
    def trailheads = points.filter(_.id == 0)
  }
  case class State(grid: Grid) {
    def paths() = {

      def getNeighbours(p: Point): List[Point] = {
        val neighbours = grid.left(p) :: Nil
        neighbours
      }

      def getPaths(points: List[Point], acc: List[List[Point]]): List[List[Point]] = {
        for {
          p <- points
          neighbours <- List(getNeighbours(p))
        } yield neighbours
      }

      def loop(thList: List[Point]):List[List[Point]] = thList match {
        case Nil => Nil
        case h :: t => {
          getPaths(List(h), List()) ::: loop(t)
        }
      }
      loop(grid.trailheads)
    }
  }
  def readLineByLine(scanner: Scanner, state: State, rowNo: Int): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
       points = l.toList.map(_.asDigit).zipWithIndex.map(t => Point(t._1, Coords(rowNo, t._2)))
        _ <- readLineByLine(scanner, state.copy(grid = Grid(points ::: state.grid.points)), rowNo + 1)
      } yield ()
    }
    else {
      val result = "bla"
      IO(s"final states is ...${state.paths}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(Grid(List())), 0)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/10.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
