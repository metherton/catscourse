package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile

import java.io.File
import java.util.Scanner
import utilsScala2.general.DebugWrapper

import scala.collection.immutable
import scala.concurrent.duration._
object Day8  extends IOApp.Simple {

  case class Grid(numberOfRows: Int, numberOfColumns: Int)
  case class Antinode(pos: Coords) {
    val id = "#"

    def offGrid(grid: Grid): Boolean = {
      pos.row >= grid.numberOfRows || pos.col >= grid.numberOfColumns || pos.row < 0 || pos.col < 0
    }
  }



  def loop(ls: List[Antenna]): List[(Antenna,Antenna)] = ls match {
    case Nil => List()
    case h :: Nil => List()
    case h :: t => t.map(a => (h, a)) ::: loop(t)
  }

  case class Antenna(id: Char, pos: Coords)
  case class State(antennas: List[Antenna], grid: Option[Grid]) {
    def antinodes: List[Antinode] = {
      def loop(ls: List[Antenna]): List[(Antenna,Antenna)] = ls match {
        case Nil => List()
        case h :: Nil => List()
        case h :: t => t.map(a => (h, a)) ::: loop(t)
      }
      val pairs = antennas.groupBy(_.id).map(el => el._1 -> loop(el._2))

      def antis(an: (Antenna, Antenna)): (Antinode, Antinode) = {
        //ANTI1(X1 - (X2 - X1), Y1 - (Y2-Y1))
        //ANTI(X2 + (X2 - X1), Y2 + (Y2 - Y1))
        (
          Antinode(Coords(an._1.pos.row - (an._2.pos.row - an._1.pos.row), an._1.pos.col - (an._2.pos.col - an._1.pos.col))),
          Antinode(Coords(an._2.pos.row + (an._2.pos.row - an._1.pos.row), an._2.pos.col + (an._2.pos.col - an._1.pos.col)))
        )
      }

      def antisb(an: (Antenna, Antenna)): List[Antinode] = {
        //ANTI1(X1 - (X2 - X1), Y1 - (Y2-Y1))
        //ANTI(X2 + (X2 - X1), Y2 + (Y2 - Y1))
        def left(lan: (Antenna, Antenna), acc: List[Antinode]): List[Antinode] = {
          if (!Antinode(Coords(lan._1.pos.row - (lan._2.pos.row - lan._1.pos.row), lan._1.pos.col - (lan._2.pos.col - lan._1.pos.col))).offGrid(grid.get)) {
            left((Antenna(lan._1.id, Coords(lan._1.pos.row - (lan._2.pos.row - lan._1.pos.row), lan._1.pos.col - (lan._2.pos.col - lan._1.pos.col))), lan._1) ,Antinode(Coords(lan._1.pos.row - (lan._2.pos.row - lan._1.pos.row), lan._1.pos.col - (lan._2.pos.col - lan._1.pos.col))) :: acc)
          } else acc

        }
        def right(ran: (Antenna, Antenna), acc: List[Antinode]): List[Antinode] = {
//          Antinode(Coords(an._2.pos.row + (an._2.pos.row - an._1.pos.row), an._2.pos.col + (an._2.pos.col - an._1.pos.col)))
          if (!Antinode(Coords(ran._2.pos.row + (ran._2.pos.row - ran._1.pos.row), ran._2.pos.col + (ran._2.pos.col - ran._1.pos.col))).offGrid(grid.get)) {
            right((ran._2, Antenna(ran._1.id, Coords(ran._2.pos.row + (ran._2.pos.row - ran._1.pos.row), ran._2.pos.col + (ran._2.pos.col - ran._1.pos.col)))) ,Antinode(Coords(ran._2.pos.row + (ran._2.pos.row - ran._1.pos.row), ran._2.pos.col + (ran._2.pos.col - ran._1.pos.col))) :: acc)
          } else acc


        }
        left(an, List()) ::: right(an, List())
//        (
//          Antinode(Coords(an._1.pos.row - (an._2.pos.row - an._1.pos.row), an._1.pos.col - (an._2.pos.col - an._1.pos.col))),
//          Antinode(Coords(an._2.pos.row + (an._2.pos.row - an._1.pos.row), an._2.pos.col + (an._2.pos.col - an._1.pos.col)))
//        )
      }

      def loopAgain(ants: List[(Antenna, Antenna)]): List[(Antinode, Antinode)] = ants match {
        case Nil => List()
        case h :: t => antis(h) :: loopAgain(t)
      }

      def loopAgainb(ants: List[(Antenna, Antenna)]): List[Antinode] = ants match {
        case Nil => List()
        //case h :: t => antis(h) :: loopAgain(t)
        case h :: t => antisb(h) ::: loopAgainb(t)
      }


      /*
     (0,6)
(0,11)
(1,3)
(2,4)
(2,10)
(3,2)
(4,9)
(5,1)
(6,3)
(7,0)
(7,7)
10,10)
(11,10)
       */

      val antins  = pairs.map(el => loopAgain(el._2))
      val antinsb  = pairs.map(el => loopAgainb(el._2))
      val bla = antins.flatten.toList
        //val bla1: List[(Antinode, Antinode)] = bla.filter(el => !el._1.offGrid(grid.get) && !el._2.offGrid(grid.get)).toList

      def fl(t: List[(Antinode, Antinode)], acc: List[Antinode]): List[Antinode] = t match {
        case Nil => acc
        case (t1,t2) :: ta => fl(ta, List(t1, t2) ::: acc)
      }


      val flatnd = fl(bla, List())
      val filterfla = flatnd.filter(el => !el.offGrid(grid.get))
      val filterflab = antennas.map(anten => Antinode(anten.pos)) ::: antinsb.flatten.toList.filter(el => !el.offGrid(grid.get))
      filterfla.toSet.toList
      filterflab.toSet.toList
    }
  }
  case class Coords(row: Int, col: Int)


  def scanLine(char: List[Char], rowNo: Int): List[Antenna] = {
    def loop(s: List[Char], ants: List[Antenna], i: Int): List[Antenna] = s match {
      case Nil => ants
      case h :: t if h != '.' => loop(t, Antenna(h, Coords(row = rowNo, col = i)) :: ants, i + 1)
      case _ :: t => loop(t, ants, i + 1)
    }
    loop(char, List(), 0)
  }

  def readLineByLine(scanner: Scanner, state: State, rowNo: Int, grid: Grid): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        ants = scanLine(l.toList, rowNo)
        _ <- readLineByLine(scanner, state.copy(antennas = ants ::: state.antennas), rowNo + 1, Grid(rowNo + 1,l.size))
      } yield ()
    }
    else {
      val finalState = state.copy(grid = Some(grid))
      IO(s"final states is ...${finalState.antinodes.size}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List[Antenna](), None), 0, Grid(0,0))
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/8.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
