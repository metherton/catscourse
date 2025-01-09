package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration._

object Day11  extends IOApp.Simple {

  case class State(nums: List[Long]) {

    def transform(): List[Long] = {
      def change(orgNums: List[Long], newNums: List[Long], myMap: Map[Long, List[Long]]): (List[Long],Map[Long, List[Long]]) = orgNums match {
        case Nil => (newNums,myMap)
        case i :: t =>
          if (myMap.contains(i)) {
            change(t, if (myMap(i).size > 1) myMap(i)(0) :: myMap(i)(1) :: newNums else myMap(i)(0) :: newNums, myMap)
          }
          else if (i == 0) {
            val newMyMap: Map[Long, List[Long]] = myMap + (0L -> List(1L))
            change(t, 1 :: newNums, newMyMap)
          }
          else if (i.toString.size % 2 == 0) {
            val parts = i.toString.splitAt(i.toString.size / 2)
           //List(parts._1.toLong, parts._2.toLong)
//            val nm: Map[Long, Long] = myMap + (parts._1.toLong -> 1)
  //          val nmm: Map[Long, Long] = if (nm.contains(parts._2.toLong)) nm.updated(parts._2.toLong, nm(parts._2.toLong) + 1L) else nm + (parts._2.toLong -> 1)
            val newMyMap: Map[Long, List[Long]] = myMap + (i -> List(parts._1.toLong, parts._2.toLong))
            change(t, parts._1.toLong :: parts._2.toLong :: newNums, newMyMap)
          } else {
            val newMyMap: Map[Long, List[Long]] = myMap + (i -> List(i * 2024))
            change(t, i * 2024 :: newNums, newMyMap)
          }
        }
      @tailrec
      def loop(i: Int, acc: List[Long], tb: Map[Long, List[Long]]): List[Long] = {
        println(s"loop $i")
        if (i >= 25) acc
        else {
          val newTb = change(acc, List(), tb)
          //val newTb = change(acc, List(), Map())
          loop(i + 1, newTb._1, newTb._2)
          //loop(i + 1, newTb._1, Map())
        }
      }
      loop(0, nums, Map[Long, List[Long]]())
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
      val result = state.transform
      IO(s"final states is ${result.size}").debug1 *> IO.unit

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
