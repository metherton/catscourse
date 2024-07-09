package part2AbstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1,2,3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Succeed(43)

  // simplified version
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor
  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor = Functor[List]

  val incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]

  val incrementedOption = optionFunctor.map(Option(2))(_ + 1)

  import cats.instances.try_._
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own Functor for a binary tree
  // hint: do not use Functor.instance
  // hint: define an object which extends Functor[Tree]
  // will need to define map function liek in trait MyFunctor
  // map function will take a tree and return another tree depending on the function we provide
  trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]


  implicit object TreeFunctor extends Functor[Tree] {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: write a shorter do10x method with extension methods
  def do10xExt[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)


  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
    println(do10x(Option(2)))
    println(do10x(Try(12)))
    println(do10x(Tree.leaf(4)))
    println(do10xExt(Tree.leaf(4)))
  }
}
