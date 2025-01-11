package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper
import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day12  extends IOApp.Simple {

  case class Regions(regions: List[Region])
  case class Coords(row: Int, col: Int)
  case class Plot(id: Char, coords: Coords)
  case class Region(plots: List[Plot])
  case class Grid(plots: List[Plot])
  case class St(ls: List[String])
  def addLines(l: List[String]): State[St, String] = State { newline =>
    (s"added $l",St(l))
  }

  def readLineByLine(scanner: Scanner, lines: List[String]): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
       // s <- IO(addLine(l))
        _ <- readLineByLine(scanner, l :: lines)
      } yield ()
    }
    else {

      val regions = Regions(List())
      import cats.data.State
      import cats.data.State._

      //val reduce: State[(Grid, Regions), String] = State(currentState => (currentState, s"added a region"))
      //val newState = reduce.run((grid, regions))

      // pure FP with states
      val firstTransformation = State((s: (Grid, Regions)) => (s._1.plots match {
        case h :: t => ((Grid(t), Regions(Region(List(h)) :: s._2.regions)), s"processed $h")
        case _ => ((s._1, s._2), "no more plots to process")
      }))
      val secondTransformation = State((s: (Grid, Regions)) => (s._1.plots match {
        case h :: t => ((Grid(t), Regions(Region(List(h)) :: s._2.regions)), s"processed $h")
        case _ => ((s._1, s._2), "no more plots to process")
      }))

      val compositeTransformation: State[(Grid, Regions), (String, String)] = firstTransformation.flatMap { firstResult =>
        secondTransformation.map(secondResult => (firstResult, secondResult))
      }

      //println(secondTransformation.map(d => d).run((grid, regions)).value)
//      val myS = for {
//        l1 <- lines
//        s <- addLines(l1)
//      } yield s
//      import cats.effect.unsafe.implicits.global

      IO(s"final states is ${lines.foldLeft((regions, 0))((r: (Regions, Int), l: String) => (Regions(List()), r._2 + 1))}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/12.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
