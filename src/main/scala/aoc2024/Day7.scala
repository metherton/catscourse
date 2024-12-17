package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper
import java.io.File
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration._

object Day7 extends IOApp.Simple {

  type CalcPair = (Long, String)

  case class State(calculations: List[Calculation])
  case class Calculation(answer: Long, values: List[Long]) {
    def getSum: Long = {
      def loop(n: List[Long], i: Int): List[List[CalcPair]] = n match {
        case Nil => List(List())
        case h :: t => {
          for {
            v <- loop(t, i + 1)
            ch <- List("*", "+", "|")
            s = (h,(if (i < values.size - 1) ch else ""))
          } yield s :: v
        }
      }

      //= loop(values, 0)
      val result: List[CalcPair] =  loop(values, 0).map(l => l.foldLeft((0L, "+"))((b, a) => b match {
        case (x, "+") => (x + a._1, a._2)
        case (x, "*") => (x * a._1, a._2)
        case (x, "|") => ((x.toString + a._1.toString).toLong, a._2)
        case (x, _) => (x, "#")
      }))
      println(s" final: ${result.toSet}")
      0

      if (result.toSet.map((x: CalcPair) => x._1).contains(answer)) answer else 0L

    }
  }
  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        ans = l.split(":")(0).toLong
        vals = l.split(":")(1).trim.split(" ").map(_.trim).map(_.toLong).toList
        _ <- readLineByLine(scanner, state.copy(calculations = Calculation(ans, vals) :: state.calculations))
      } yield ()
    }
    else {
      val res = state.calculations.map(c => c.getSum).sum
      IO(s"final total is ...$res").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List[Calculation]()))
      }
  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/7.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
