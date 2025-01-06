package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day11  extends IOApp.Simple {

  case class State(nums: List[Long]) {
    def transform(): List[Long] = {
      def change(i: Long): List[Long] = {
        if (i == 0) List(1)
        else if (i.toString.size % 2 == 0) {
          val parts = i.toString.splitAt(i.toString.size / 2)
          List(parts._1.toLong, parts._2.toLong)
        } else List(i * 2024)
      }
      def loop(i: Int, acc: List[Long]): List[Long] = {
        println(i)
        if (i >= 25) acc
        else {
          val newValues = for {
            num <- acc
            chg = change(num)
          }  yield chg
          loop(i + 1, newValues.flatten)
        }
      }
      loop(0, nums)
    }
  }

  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        ls <- IO(l.split(" ").toList.map(_.toLong))
        _ <- readLineByLine(scanner, state.copy(nums = ls))
      } yield ()
    }
    else {
      val result = "bla"
      IO(s"final states is ...${state.transform.size}").debug1 *> IO.unit

      //IO(s"final states grouped is ...${state.paths.groupBy(lf => (lf(0), lf(9))).size}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List()))
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/11.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
