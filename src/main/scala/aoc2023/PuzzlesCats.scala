package aoc2023

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.openFileScanner

import java.util.Scanner
import utils._
import scala.concurrent.duration._

object PuzzlesCats extends IOApp.Simple {

  def readLineByLine(scanner: Scanner, acc: Int): IO[Unit] = {
    def findNumber(s: String): String = {
      s.find(i => i.isDigit).head.toString + s.findLast(i => i.isDigit).head.toString
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        r = findNumber(l).toInt
        _ <- readLineByLine(scanner, acc + r)
      } yield ()
    }
    else {
      IO(s"final total is $acc").debug1 *> IO.unit
    }
  }

  def readLineByLine2(scanner: Scanner, acc: Map[Int, Int], i: Int): IO[Unit] = {
    def updateAcc(s: String, a: Map[Int, Int]): Map[Int, Int] = {
      val lists = s
      val parts = s.split(":")(1).split("\\|").map(s => s.split(" ").filterNot(_ == "").map(_.toInt))
      //println(s)
      val n = parts(0).toList.intersect(parts(1).toList).length
      val map1 = a + (if (a.contains(i)) {
        //println("should not come in here")
        i -> (a.get(i).get + 1)
      } else {
        i -> 1
      })
      val newMap: Map[Int, Int] = ((1 to n).map(j => if (map1.contains(i + j)) {
        i + j -> (map1.get(i + j).get + map1.get(i).get)
      } else {
    //    i + j -> 1
        i + j -> map1.get(i).get
      })).toMap
      val evenNewerMap: Map[Int, Int] = map1 ++ newMap
      //println(evenNewerMap.map(_._2).sum)
      evenNewerMap
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine())//.debug1
        _ <- IO.sleep(1.millis)
        n = updateAcc(l, acc)
        _ <- readLineByLine2(scanner, n, i + 1)
      } yield ()
    }
    else {
      val t = acc.map(_._2).sum
      IO(s"final total is $t").debug1 *> IO.unit
    }
  }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner, 0)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  def bracketReadFile4(path: String): IO[Unit] =
    IO(s"opening file at $path").debug1 *>
      openFileScanner(path).bracket { scanner =>
        readLineByLine2(scanner, Map[Int, Int](), 1)
      } { scanner =>
        IO(s"closing file at $path").debug1 *> IO(scanner.close())
      }

  override def run: IO[Unit] = {
    //bracketReadFile("src/main/resources/1.txt").void
    bracketReadFile4("src/main/resources/4.txt").void
  }
}
