package part4typeclasses

import cats.{Eval, Monoid}
import cats.syntax.monoid

object Folding {

  // TODO: implement all in types of fold left & foldRight
  object ListExercises {

    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)
    def flatMap[A, B](list: List[A])(f: A => List[B]) = list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)((a, b) => monoid.combine(a, b))

  }

  import cats.Foldable
  import cats.instances.list._
  val sum = Foldable[List].foldLeft(List(1,2,3), 0)(_ + _) // 6

  import cats.instances.option._

  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32


  // foldRight is stack safe regardless of your container
  val sumRight = Foldable[List].foldRight(List(1,2,3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  // convenience
  import cats.instances.int._
  val anotherSum = Foldable[List].combineAll(List(1,2,3)) // implicit Monoid[Int]
  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(_.toString)// implicict Monoid[String] => 1,2,3

  // nesting
  import cats.instances.vector._
  val intsNested = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]) combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._

  val sum3 = List(1,2,3).combineAll // requires Foldable[List] Monoid[Int]

  val mappedConcat2 = List(1,2,3).foldMap(_.toString)
  def main(args: Array[String]): Unit = {
    import ListExercises._
    println(map(List(1,2,3))(_ * 3))
    println(flatMap(List(1,2,3))(a => List(1, a * 3)))

    println(combineAll(List(1,2,3,4))) // Monoid[Int]
  }
}
