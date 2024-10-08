package part1intro

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object CatsTypeClasses {

  /*
    - applicative
    - functor
    - flatMap
    - monad
    - apply
    - applicativeError/monadError
    - traverse

   */


  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B] // F[A] could be a list of ints..and it could get mapped to a list of strings
  }

  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.future._

  val listFunctor = Functor[List]
  val optFunctor = Functor[Option]


  // generalizable "mapping" APIs
  def increment[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = {
    functor.map(container)(_ + 1)
  }

  import cats.syntax.functor._

  def increment_v2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)


  // applicative - the ability to wrap types
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A] // can be thought of as a sort of constructor of the type argument F
  }


  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43)

  import cats.syntax.applicative._ // import the pure extension method
  val aSimpleList_v2: List[Int] = 43.pure[List]


  // flatMap - ability to chain multiple computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](container: F[A], f : A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]
  import cats.syntax.flatMap._ // has flatMap extension method

  def crossProduct[F[_] : FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b))) // FlatMap extends Functor so we also have access to the map method




  // MONAD - applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](ma: F[A])(f: A => B): F[B] = {
      flatMap(ma, (a: A) => pure(f(a)))
    }
  }


  import cats.Monad

  val monadList = Monad[List]

  def crossProduct_v2[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // error like type classes
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError

  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val applicativeErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue: ErrorOr[Int] = applicativeErrorEither.pure(42)
  val failedValue: ErrorOr[Int] = applicativeErrorEither.raiseError("Something failed")

  import cats.syntax.applicativeError._ // raiseError extension method
  val failedValue2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]


  // monad error
  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]


  // traverse
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  // turn nested wrappers inside out
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(43))
  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1,2,3))(x => Option(x))

  import cats.syntax.traverse._
  val optionList2: Option[List[Int]] = List(1,2,3).traverse(x => Option(x))

  def main(args: Array[String]): Unit = {
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    println(increment(List(1, 2,3)))
    println(increment(Option(1)))
    println(Await.result(Future(1), 30.second))
  }
}
