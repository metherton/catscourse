package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.{getResourceFromFile, openFileScanner}
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

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
