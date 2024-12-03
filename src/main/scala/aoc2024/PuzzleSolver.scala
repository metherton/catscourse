package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.{getResourceFromFile, openFileScanner}
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.matching.Regex

object PuzzleSolver1a extends IOApp.Simple {

  def readLineByLine1a(scanner: Scanner, acc: List[(Int, Int)]): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        t = l.split("  ").map(_.trim).map(_.toInt)
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine1a(scanner, (t(0), t(1)) :: acc)
      } yield ()
    }
    else {
      val f = acc.map(_._1).sorted
      val s = acc.map(_._2).sorted
      val z = f.zip(s)
      val diffs = z.map(t => (t._2 - t._1).abs).sum
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile1a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine1a(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/1.txt")

    resourceReadFile1a(inputFile.getAbsolutePath)
  }

}


object PuzzleSolver1b extends IOApp.Simple {

  def readLineByLine1b(scanner: Scanner, acc: List[(Int, Int)]): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        t = l.split("  ").map(_.trim).map(_.toInt)
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine1b(scanner, (t(0), t(1)) :: acc)
      } yield ()
    }
    else {
      val f = acc.map(_._1).sorted
      val fMap = acc.map(_._1).sorted.distinct.map(d => (d, 0)).toMap
      val s = acc.map(_._2).sorted
      println(fMap)
      val newMap = for {
        x <- fMap
        count = s.filter(_ == x._1).length
      } yield (x._1, count)
      println(newMap)
      val newF = f.map(s => (s, newMap(s)))
      val newFSup = newF.map((s) => s._1 * s._2).sum
      println(newFSup)

      //      val s = acc.map(_._2).sorted
      //      val z = f.zip(s)
      //      val diffs = z.map(t => (t._2 - t._1).abs).sum
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile1b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine1b(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/1.txt")

    resourceReadFile1b(inputFile.getAbsolutePath)
  }
}
object PuzzleSolver2a extends IOApp.Simple {


  def checkNumbers(n: List[Int]): Boolean = {
    @tailrec
    def loop(acc: List[Int], lessThan: Boolean): Boolean = acc match {
      case Nil => true
      case _ :: Nil => true
      case h1 :: h2 :: s if (h1 <= h2 && lessThan && ((h1 - h2).abs >= 1 && (h1 - h2).abs <= 3)) =>  loop(h2 :: s, true)
      case h1 :: h2 :: s if (h1 >= h2 && !lessThan && ((h1 - h2).abs >= 1 && (h1 - h2).abs <= 3)) =>  loop(h2 :: s, false)
      case _ => false
    }
    if (n.length >= 2) {
      if (n(0) < n(1)) loop(n, true) else loop(n, false)
    } else true
  }

  def readLineByLine2a(scanner: Scanner, acc: List[String]): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        t = l.split(" ").map(_.trim).map(_.toInt).toList
        isSafe = checkNumbers(t)
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine2a(scanner, if (isSafe) l :: acc else acc)
      } yield ()
    }
    else {
      //      val s = acc.map(_._2).sorted
      //      val z = f.zip(s)
      //      val diffs = z.map(t => (t._2 - t._1).abs).sum
      println(acc.length)
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile2a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine2a(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/2.txt")

    resourceReadFile2a(inputFile.getAbsolutePath)
  }

}


object PuzzleSolver2b extends IOApp.Simple {


  def checkNumbers(n: List[Int], i: Int): Boolean = {
    def withinRange(h1: Int, h2: Int): Boolean = {
      if ((h1 - h2).abs >= 1 && (h1 - h2).abs <= 3) true else false
    }
    def isOrdered(h1: Int, h2: Int, lessThan: Boolean): Boolean = {
      if (lessThan) {
        h1 < h2
      } else {
        h1 > h2
      }
    }
    def allowedDiscrepancies(count: Int): Boolean = {
      count < 1
    }

    @tailrec
    def loop(acc: List[Int], lessThan: Boolean, count: Int): Boolean = acc match {
      // list is empty
      case Nil => true
      case _ :: Nil => true
      case h1 :: h2 :: Nil if (isOrdered(h1, h2, lessThan)  && withinRange(h1, h2))  || allowedDiscrepancies(count) => true
      case h1 :: h2 :: h3 :: s if ((isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h2, h3, lessThan) && withinRange(h2, h3))) => loop(h2 :: h3 :: s, lessThan, count)
      case h1 :: h2 :: h3 :: s if (!(isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h1, h3, lessThan) && withinRange(h1, h3)) && allowedDiscrepancies(count)) => loop(h1 :: h3 :: s, lessThan, count + 1)
      case h1 :: h2 :: h3 :: s if ((isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && !(isOrdered(h2, h3, lessThan) && withinRange(h2, h3)) && allowedDiscrepancies(count)) => loop(h1 :: h2 :: s, lessThan, count + 1)
      case h1 :: h2 :: h3 :: s if (!(isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h2, h3, lessThan) && withinRange(h2, h3)) && allowedDiscrepancies(count)) => loop(h2 :: h3 :: s, lessThan, count + 1)
      case _ => false
    }
    if (n.length >= 4) {
      if (((n(0) < n(1)) && (n(1) < n(2)) || (n(0) < n(1)) && (n(2) < n(3))  || (n(1) < n(2)) && (n(2) < n(3)))) loop(n, true, i) else loop(n, false, i)
    } else {
      println("some array with only 4 or less")
      true
    }
  }

  def readLineByLine2b(scanner: Scanner, acc: List[String]): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        t = l.split(" ").map(_.trim).map(_.toInt).toList
        isSafe = checkNumbers(t,0)
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine2b(scanner, if (isSafe) l :: acc else acc)
      } yield ()
    }
    else {
      //      val s = acc.map(_._2).sorted
      //      val z = f.zip(s)
      //      val diffs = z.map(t => (t._2 - t._1).abs).sum
      println(acc.length)
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile2b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine2b(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/2.txt")

    resourceReadFile2b(inputFile.getAbsolutePath)
  }

}


object PuzzleSolver3a extends IOApp.Simple {


  def checkNumbers(n: List[Int], i: Int): Boolean = {
    def withinRange(h1: Int, h2: Int): Boolean = {
      if ((h1 - h2).abs >= 1 && (h1 - h2).abs <= 3) true else false
    }
    def isOrdered(h1: Int, h2: Int, lessThan: Boolean): Boolean = {
      if (lessThan) {
        h1 < h2
      } else {
        h1 > h2
      }
    }
    def allowedDiscrepancies(count: Int): Boolean = {
      count < 1
    }

    @tailrec
    def loop(acc: List[Int], lessThan: Boolean, count: Int): Boolean = acc match {
      // list is empty
      case Nil => true
      case _ :: Nil => true
      case h1 :: h2 :: Nil if (isOrdered(h1, h2, lessThan)  && withinRange(h1, h2))  || allowedDiscrepancies(count) => true
      case h1 :: h2 :: h3 :: s if ((isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h2, h3, lessThan) && withinRange(h2, h3))) => loop(h2 :: h3 :: s, lessThan, count)
      case h1 :: h2 :: h3 :: s if (!(isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h1, h3, lessThan) && withinRange(h1, h3)) && allowedDiscrepancies(count)) => loop(h1 :: h3 :: s, lessThan, count + 1)
      case h1 :: h2 :: h3 :: s if ((isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && !(isOrdered(h2, h3, lessThan) && withinRange(h2, h3)) && allowedDiscrepancies(count)) => loop(h1 :: h2 :: s, lessThan, count + 1)
      case h1 :: h2 :: h3 :: s if (!(isOrdered(h1, h2, lessThan) && withinRange(h1, h2)) && (isOrdered(h2, h3, lessThan) && withinRange(h2, h3)) && allowedDiscrepancies(count)) => loop(h2 :: h3 :: s, lessThan, count + 1)
      case _ => false
    }
    if (n.length >= 4) {
      if (((n(0) < n(1)) && (n(1) < n(2)) || (n(0) < n(1)) && (n(2) < n(3))  || (n(1) < n(2)) && (n(2) < n(3)))) loop(n, true, i) else loop(n, false, i)
    } else {
      println("some array with only 4 or less")
      true
    }
  }

  def readLineByLine3a(scanner: Scanner, acc: List[Int]): IO[Unit] = {
    def getAmount(l: String): Int = {
      val mulPattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r
      val result = mulPattern.findAllIn(l).toList
      val newr = for {
        a <- result
        r = a.substring(4).substring(0, a.length - 5).split(",").map(_.toInt).toList.product
      } yield r
      newr.sum
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        amount = getAmount(l)
        _ <- readLineByLine3a(scanner, amount :: acc)
      } yield ()
    }
    else {
      //      val s = acc.map(_._2).sorted
      //      val z = f.zip(s)
      //      val diffs = z.map(t => (t._2 - t._1).abs).sum
      println(acc.sum)
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile3a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine3a(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/3.txt")

    resourceReadFile3a(inputFile.getAbsolutePath)
  }

}

object PuzzleSolver3b extends IOApp.Simple {


  def readLineByLine3b(scanner: Scanner, acc: List[Int]): IO[Unit] = {
    def getAmount(l: String): Int = {
      val mulPattern: Regex = """(mul\(\d{1,3},\d{1,3}\))|(don't\(\))|(do\(\))""".r
      val result = mulPattern.findAllIn(l).toList
      val newr = for {
        a <- result
//        r = a.substring(4).substring(0, a.length - 5).split(",").map(_.toInt).toList.product
      } yield a

      //newr.sum
      1
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        amount = getAmount(l)
        _ <- readLineByLine3b(scanner, amount :: acc)
      } yield ()
    }
    else {
      println(acc.sum)
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile3b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine3b(scanner, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/3.txt")

    resourceReadFile3b(inputFile.getAbsolutePath)
  }

}