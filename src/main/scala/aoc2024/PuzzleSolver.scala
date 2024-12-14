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


  def readLineByLine3b(scanner: Scanner, acc: List[Int], initialValue: Boolean): IO[Unit] = {

    def getAmount(l: String, initialValue: Boolean): (Boolean,Int) = {
      val mulPattern: Regex = """(mul\(\d{1,3},\d{1,3}\))|(don't\(\))|(do\(\))""".r
      val result = mulPattern.findAllIn(l).toList
      val bla = result.foldLeft((initialValue,0))((a, b) => {
        if (b != "do()" && b != "don't()") {
          if (a._1) {
            val p = b.substring(4).substring(0, b.length - 5).split(",").map(_.toInt).toList.product
            (true, p + a._2)
          } else {
            (false, a._2)
          }
        } else if (b == "do()") {
          (true, a._2)
        } else {
          (false, a._2)
        }
      })
      bla
    }
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        amount = getAmount(l, initialValue)
        _ <- readLineByLine3b(scanner, amount._2 :: acc, amount._1)
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
          readLineByLine3b(scanner, List(), true)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/3.txt")

    resourceReadFile3b(inputFile.getAbsolutePath)
  }

}

object PuzzleSolver4a extends IOApp.Simple {

  def readLineByLine4a(scanner: Scanner, count: Int, acc: List[String]): IO[Unit] = {

    def findHorizontal(l: String): Int = {
      val hpF: Regex = """(XMAS)""".r
      val hpR: Regex = """(SAMX)""".r
      hpF.findAllIn(l).toList.length + hpR.findAllIn(l).toList.length
    }

    def findOthers(list: List[String]): Int = {
      def findVertical(i: Int): Int = {
        val rightOrder = if (list(0).charAt(i) == 'X' && list(1).charAt(i) == 'M' && list(2).charAt(i) == 'A' && list(3).charAt(i) == 'S') 1 else 0
        val reverseOrder = if (list(3).charAt(i) == 'X' && list(2).charAt(i) == 'M' && list(1).charAt(i) == 'A' && list(0).charAt(i) == 'S') 1 else 0
        rightOrder + reverseOrder
      }
      def findDiagonal(i: Int): Int = {
        val rightDownNormal = if (list(0).charAt(i) == 'X' && list(1).charAt(i+1) == 'M' && list(2).charAt(i+2) == 'A' && list(3).charAt(i+3) == 'S') 1 else 0 // worked
        val rightDownReverse = if (list(0).charAt(i) == 'S' && list(1).charAt(i + 1) == 'A' && list(2).charAt(i + 2) == 'M' && list(3).charAt(i + 3) == 'X') 1 else 0
        val leftDownNormal = if (list(0).charAt(i+3) == 'X' && list(1).charAt(i+2) == 'M' && list(2).charAt(i+1) == 'A' && list(3).charAt(i) == 'S') 1 else 0 // worked
        val leftDownReverse = if (list(0).charAt(i + 3) == 'S' && list(1).charAt(i + 2) == 'A' && list(2).charAt(i+1) == 'M' && list(3).charAt(i) == 'X') 1 else 0
        rightDownNormal + rightDownReverse + leftDownNormal + leftDownReverse
      }
      if (list.length == 4) {
        val r = for {
          i <- 0 until (list(0)).length
          vertical = findVertical(i)
        } yield vertical
        val d = for {
          i <- 0 until list(0).length - 3
          diagonal = findDiagonal(i)
        } yield diagonal
        r.sum + d.sum
      } else {
        0
      }
    }

    val others = findOthers(acc)

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        horizontalCount = findHorizontal(l)
        _ <- readLineByLine4a(scanner, count + horizontalCount + others, (l :: acc).take(4))
      } yield ()
    }
    else {
      val total = count + others
      println(s"Total: $total")
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile4a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine4a(scanner, 0, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/4.txt")

    resourceReadFile4a(inputFile.getAbsolutePath)
  }

}

object PuzzleSolver4b extends IOApp.Simple {

  def readLineByLine4b(scanner: Scanner, count: Int, acc: List[String]): IO[Unit] = {

    def findOthers(list: List[String]): Int = {
      def findDiagonal(i: Int): Int = {
        val rightDownNormal = if (list(0).charAt(i) == 'M' && list(1).charAt(i+1) == 'A' && list(2).charAt(i+2) == 'S') 1 else 0 // worked
        val rightDownReverse = if (list(0).charAt(i) == 'S' && list(1).charAt(i + 1) == 'A' && list(2).charAt(i + 2) == 'M') 1 else 0
        val leftDownNormal = if (list(0).charAt(i+2) == 'M' && list(1).charAt(i+1) == 'A' && list(2).charAt(i) == 'S') 1 else 0 // worked
        val leftDownReverse = if (list(0).charAt(i + 2) == 'S' && list(1).charAt(i + 1) == 'A' && list(2).charAt(i) == 'M') 1 else 0
        if ((rightDownNormal + rightDownReverse + leftDownNormal + leftDownReverse) == 2) 1 else 0
      }
      if (list.length == 3) {
        val d = for {
          i <- 0 until list(0).length - 2
          diagonal = findDiagonal(i)
        } yield diagonal
        d.sum
      } else {
        0
      }
    }

    val others = findOthers(acc)

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine4b(scanner, count + others, (l :: acc).take(3))
      } yield ()
    }
    else {
      val total = count + others
      println(s"Total: $total")
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile4b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine4b(scanner, 0, List())
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/4.txt")

    resourceReadFile4b(inputFile.getAbsolutePath)
  }

}

object PuzzleSolver5a extends IOApp.Simple {

  def readLineByLine5a(scanner: Scanner, breakReached: Boolean, rules: List[(Int, Int)], updates: List[String], acc: Int): IO[Unit] = {

    def determineValue(l: String): Int = {
      val updates = l.split(",").toList.map(_.toInt)
      val result = updates.foldLeft((true, updates.tail))((acc, x) => {
        val mappedLeft = rules.filter((tup) => tup._1 == x)
        val mappedRight = rules.filter((tup) => tup._2 == x)
        val stillTrue = acc._2.toSet.intersect(mappedLeft.map(_._2).toSet).size == acc._2.toSet.size
        val notInOther = acc._2.toSet.intersect(mappedRight.map(_._1).toSet).size == 0
        (stillTrue && notInOther && acc._1, if (acc._2.size > 0)acc._2.tail else List())
      })
      if (result._1) updates(updates.size / 2) else 0
    }

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        c = if (breakReached) determineValue(l) else 0
        breakNowReached = if (l.isBlank || breakReached) true else false
        _ <- IO.sleep(1.millis)
        tup = if (!breakNowReached) {val t = l.split('|');val tInt = (t(0).toInt, t(1).toInt);tInt;} else (0, 0)
        _ <- readLineByLine5a(scanner, breakNowReached, if (!breakNowReached) tup :: rules else rules, updates, acc + c)
      } yield ()
    }
    else {
      println(s"Total: $acc")
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile5a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine5a(scanner, false, List(), List(), 0)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/5.txt")

    resourceReadFile5a(inputFile.getAbsolutePath)
  }

}

object PuzzleSolver5b extends IOApp.Simple {

  def readLineByLine5b(scanner: Scanner, breakReached: Boolean, rules: List[(Int, Int)], updates: List[String], acc: Int): IO[Unit] = {

    def determineAlternativeValue(updates: List[Int]): Int = {
      val result = updates.foldLeft(List[Int]())((b, x) => {
        val mappedLeft = rules.filter((tup) => tup._1 == x)
        val mappedRight = rules.filter((tup) => tup._2 == x)
        def insert(acc: List[Int]): List[Int] = acc match {
          case Nil => x :: acc
          case h :: t if (mappedRight.map(y => y._1).contains(h)) => h :: insert(t)
          case h :: t => x :: acc
        }
        val newAcc = insert(b)
        newAcc
      })
      val r = result(result.size / 2)
      r
    }

    def determineValue(l: String): Int = {
      val updates = l.split(",").toList.map(_.toInt)
      val result = updates.foldLeft((true, updates.tail))((acc, x) => {
        val mappedLeft = rules.filter((tup) => tup._1 == x)
        val mappedRight = rules.filter((tup) => tup._2 == x)
        val stillTrue = acc._2.toSet.intersect(mappedLeft.map(_._2).toSet).size == acc._2.toSet.size
        val notInOther = acc._2.toSet.intersect(mappedRight.map(_._1).toSet).size == 0
        (stillTrue && notInOther && acc._1, if (acc._2.size > 0)acc._2.tail else List())
      })
      if (!result._1) determineAlternativeValue(updates) else 0
    }

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        c = if (breakReached) determineValue(l) else 0
        breakNowReached = if (l.isBlank || breakReached) true else false
        _ <- IO.sleep(1.millis)
        tup = if (!breakNowReached) {val t = l.split('|');val tInt = (t(0).toInt, t(1).toInt);tInt;} else (0, 0)
        _ <- readLineByLine5b(scanner, breakNowReached, if (!breakNowReached) tup :: rules else rules, updates, acc + c)
      } yield ()
    }
    else {
      println(s"Total: $acc")
      IO(s"final total is ...").debug1 *> IO.unit
    }
  }

  def resourceReadFile5b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine5b(scanner, false, List(), List(), 0)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/5.txt")
    resourceReadFile5b(inputFile.getAbsolutePath)
  }
}

object PuzzleSolver6a extends IOApp.Simple {

  case class State(numRows: Int, numColumns: Int, startPosition: (Int, Int), obstacles: Map[Int, List[Int]])
  def readLineByLine6a(scanner: Scanner, state: State) : IO[Unit] = {

    def getStartPosition(chars: List[Char]): (Boolean, Int) = {
      def loop(s: List[Char], found: Boolean, i: Int): (Boolean, Int) = s match {
        case Nil => (found, i)
        case h :: t if h == '^' => (true, i)
        case h :: t => loop(t, found, i + 1)
      }
      loop(chars, false, 0)
    }

    def getObstacles(chars: List[Char]): List[Int] = {
      def loop(s: List[Char], acc: List[Int], i: Int): List[Int] = s match {
        case Nil => acc
        case h :: t if h == '#' => loop(t, i :: acc, i + 1)
        case h :: t => loop(t, acc, i + 1)
      }
      loop(chars, List(), 0)
    }

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        startPos = getStartPosition(l.toList)
        obs = state.numRows -> getObstacles(l.toList)
        _ <- readLineByLine6a(scanner, state.copy(numRows = state.numRows + 1, numColumns = l.size, startPosition = if (startPos._1) (state.numRows, startPos._2) else state.startPosition, obstacles = state.obstacles + obs))
      } yield ()
    }
    else {
      println(s"Total: $state")
      // Now we need to calculate the route
      def move(position: (Int, Int), points: Set[(Int, Int)], direction: String): Set[(Int, Int)] = {

        def determineNewPosition(p: (Int, Int), direction: String): ((Int, Int), String) = {
          if (direction == "up") {
            if (state.obstacles(position._1 - 1).contains(position._2)) {
              val newPosition = (position._1, position._2 + 1)
              (newPosition, "right")
            } else {
              val newPosition = (position._1 - 1, position._2)
              (newPosition, "up")
            }
          } else if (direction == "right") {
            if (state.obstacles(position._1).contains(position._2 + 1)) {
              val newPosition = (position._1 + 1, position._2)
              (newPosition, "down")
            } else {
              val newPosition = (position._1, position._2 + 1)
              (newPosition, "right")
            }
          } else if (direction == "left") {
            if (state.obstacles(position._1).contains(position._2 - 1)) {
              val newPosition = (position._1 - 1, position._2)
              (newPosition, "up")
            } else {
              val newPosition = (position._1, position._2 - 1)
              (newPosition, "left")
            }
          } else {
            if (state.obstacles(position._1 + 1).contains(position._2)) {
              val newPosition = (position._1, position._2 - 1)
              (newPosition, "left")
            } else {
              val newPosition = (position._1 + 1, position._2)
              (newPosition, "down")
            }
          }
        }

        if ((position._1 >= state.numRows - 1 && direction == "down") || (position._2 >= state.numColumns && direction == "right") || (position._1 - 1 < 0 && direction == "up") || (position._2 < 0 && direction == "left")) {
          points
        } else {
          if (direction == "up") {
            if (state.obstacles(position._1 - 1).contains(position._2)) {
              val newPosition = (position._1, position._2 + 1)
              move(newPosition, points + newPosition, "right")
            } else {
              val newPosition = (position._1 - 1, position._2)
              move(newPosition, points + newPosition, "up")
            }
          } else if (direction == "right") {
            if (state.obstacles(position._1).contains(position._2 + 1)) {
              val newPosition = (position._1 + 1, position._2)
              move(newPosition, points + newPosition, "down")
            } else {
              val newPosition = (position._1, position._2 + 1)
              move(newPosition, points + newPosition, "right")
            }
          } else if (direction == "left") {
            if (state.obstacles(position._1).contains(position._2 - 1)) {
              val newPosition = (position._1 - 1, position._2)
              move(newPosition, points + newPosition, "up")
            } else {
              val newPosition = (position._1, position._2 - 1)
              move(newPosition, points + newPosition, "left")
            }
          } else {
            if (state.obstacles(position._1 + 1).contains(position._2)) {
              val newPosition = (position._1, position._2 - 1)
              move(newPosition, points + newPosition, "left")
            } else {
              val newPosition = (position._1 + 1, position._2)
              move(newPosition, points + newPosition, "down")
            }
          }
        }
      }

      val count = move(state.startPosition, Set[(Int, Int)](), "up")

      IO(s"final total is ...${count.size}").debug1 *> IO.unit
    }
  }

  def resourceReadFile6a(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine6a(scanner, State(0, 0, (0,0), Map()))
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/6.txt")
    resourceReadFile6a(inputFile.getAbsolutePath)
  }
}

object PuzzleSolver6b extends IOApp.Simple {

  case class State(numRows: Int, numColumns: Int, startPosition: (Int, Int), obstacles: List[(Int, Int)])
  def readLineByLine6b(scanner: Scanner, state: State) : IO[Unit] = {

    def getStartPosition(chars: List[Char]): (Boolean, Int) = {
      def loop(s: List[Char], found: Boolean, i: Int): (Boolean, Int) = s match {
        case Nil => (found, i)
        case h :: t if h == '^' => (true, i)
        case h :: t => loop(t, found, i + 1)
      }
      loop(chars, false, 0)
    }

    def getObstacles(chars: List[Char]): List[Int] = {
      def loop(s: List[Char], acc: List[Int], i: Int): List[Int] = s match {
        case Nil => acc
        case h :: t if h == '#' => loop(t, i :: acc, i + 1)
        case h :: t => loop(t, acc, i + 1)
      }
      loop(chars, List(), 0)
    }

    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        startPos = getStartPosition(l.toList)
        obs = getObstacles(l.toList).map(i => (state.numRows, i))
        _ <- readLineByLine6b(scanner, state.copy(numRows = state.numRows + 1, numColumns = l.size, startPosition = if (startPos._1) (state.numRows, startPos._2) else state.startPosition, obstacles = obs ::: state.obstacles))
      } yield ()
    }
    else {
      println(s"Total: $state")
      // Now we need to calculate the route
      def move(position: ((Int, Int), String), points: List[((Int, Int), String)], direction: String, count: Int): Int = {

        def checkExists(p: ((Int, Int), String)): Boolean = {
          val possibleParallelPoints = p._2 match {
            case "up" => {
              val ps = for {
                poss <- Range.inclusive(if (points.filter(x => x._2 == "up" && x._1._2 == p._1._2 && x._1._1 < p._1._1).map(_._1._1).size > 0) points.filter(x => x._2 == "up" && x._1._2 == p._1._2 && x._1._1 < p._1._1).map(_._1._1).sorted.reverse.head + 1 else p._1._1 + 1, p._1._1).toList
                vec = (poss, p._1._2)
              } yield vec
              ps
            }
            case "right" => {
              val ps = for {
                poss <- Range.inclusive(p._1._2, if (points.filter(x => x._2 == "right" && x._1._1 == p._1._1 && x._1._2 > p._1._2).map(_._1._2).size > 0) points.filter(x => x._2 == "right" && x._1._1 == p._1._1 && x._1._2 > p._1._2).map(_._1._2).sorted.head else 0)
                vec = (p._1._1, poss)
              } yield vec
              ps
            }
            case "left" => {
              val ps = for {
                poss <- Range.inclusive(if (points.filter(x => x._2 == "left" && x._1._1 == p._1._1 && x._1._2 < p._1._2).map(_._1._2).size > 0) points.filter(x => x._2 == "left" && x._1._1 == p._1._1 && x._1._2 < p._1._2).map(_._1._2).sorted.reverse.head + 1 else p._1._2 + 1, p._1._2)
                vec = (p._1._1, poss)
              } yield vec
              ps
            }

            case _ => {
              val ps = for {
                poss <- Range.inclusive(p._1._1, if (points.filter(x => x._2 == "down" && x._1._2 == p._1._2 && x._1._1 > p._1._1).map(_._1._1).size > 0) points.filter(x => x._2 == "down" && x._1._2 == p._1._2 && x._1._1 > p._1._1).map(_._1._1).sorted.head else 0).toList
                vec = (poss, p._1._2)
              } yield vec
              ps
            }
          }
          val inters = possibleParallelPoints.intersect(state.obstacles)


          points.filter(el => el._2 == p._2 && p._1._1 == el._1._1 && p._1._2 == el._1._2).size > 0 || (possibleParallelPoints.size > 0 && inters.size == 0)



        }

        if ((position._1._1 >= state.numRows - 1 && direction == "down") || (position._1._2 >= state.numColumns && direction == "right") || (position._1._1 - 1 < 0 && direction == "up") || (position._1._2 < 0 && direction == "left")) {
          count
        } else {
          if (direction == "up") {
            if (state.obstacles.contains((position._1._1 - 1, position._1._2))) {
              val newPosition = ((position._1._1, position._1._2 + 1), "right")
              move(newPosition, newPosition :: points, "right", count)
            } else {
              val newPosition = ((position._1._1 - 1, position._1._2),"up")
              // what if the new position was a barrier though..calculate what the new entry would be then check if it exists
              val altPosition = ((position._1._1, position._1._2 + 1), "right")
              val exists = checkExists(altPosition)
              move(newPosition, newPosition :: points, "up", if (exists) count + 1 else count)
            }
          } else if (direction == "right") {
            if (state.obstacles.contains((position._1._1,position._1._2 + 1))) {
              val newPosition = ((position._1._1 + 1, position._1._2),"down")
              move(newPosition, newPosition :: points, "down", count)
            } else {
              val newPosition = ((position._1._1, position._1._2 + 1),"right")
              // what if the new position was a barrier though..calculate what the new entry would be then check if it exists
              val altPosition = ((position._1._1 + 1, position._1._2), "down")
              val exists = checkExists(altPosition)
              move(newPosition, newPosition :: points, "right", if (exists) count + 1 else count)
            }
          } else if (direction == "left") {
            if (state.obstacles.contains((position._1._1,position._1._2 - 1))) {
              val newPosition = ((position._1._1 - 1, position._1._2),"up")
              move(newPosition, newPosition :: points, "up", count)
            } else {
              val newPosition = ((position._1._1, position._1._2 - 1),"left")
              // what if the new position was a barrier though..calculate what the new entry would be then check if it exists
              val altPosition = ((position._1._1 - 1, position._1._2), "up")
              val exists = checkExists(altPosition)
              move(newPosition, newPosition :: points, "left", if (exists) count + 1 else count)
            }
          } else {
            if (state.obstacles.contains((position._1._1 + 1,position._1._2))) {
              val newPosition = ((position._1._1, position._1._2 - 1),"left")
              move(newPosition, newPosition :: points, "left", count)
            } else {
              val newPosition = ((position._1._1 + 1, position._1._2),"down")
              // what if the new position was a barrier though..calculate what the new entry would be then check if it exists
              val altPosition = ((position._1._1, position._1._2 - 1), "left")
              val exists = checkExists(altPosition)
              move(newPosition, newPosition :: points, "down", if (exists) count + 1 else count)
            }
          }
        }
      }

      val count = move((state.startPosition,"up"), List[((Int, Int), String)](), "up", 0)

      IO(s"final total is ...${count}").debug1 *> IO.unit
    }
  }

  def resourceReadFile6b(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine6b(scanner, State(0, 0, (0,0), List()))
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/6.txt")
    resourceReadFile6b(inputFile.getAbsolutePath)
  }
}




