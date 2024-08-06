package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal

  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string")
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life",42))

  import cats.instances.list._ // imports a Monad[List]
  val aTupledList = Semigroupal[List].product(List(1,2), List("a", "b"))

  // TODO: implement with monads
  import cats.Monad
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
  }
  //or
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap
  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

//  trait MyMonad[M[_]] extends MySemigroupal[M] {
//    def pure[A](value: A): M[A]
//    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
//
//    // TODO: Implement this
//    def map[A, B](ma: M[A])(f: A => B): M[B] = {
//      flatMap(ma)(a => pure(f(a)))
//    }
//    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = {
//      flatMap(fa)(a => map(fb)(b => (a, b)))
//    }
//  }

  // MONADS extend SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]

  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This cant be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "something else wrong")),
    Left(List("this cant be right"))
  )


  // Associativity - m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2L define a Semigroupal[List] which does a zip

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)
  }



  def main(args: Array[String]): Unit = {
    println(invalidCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1,2), List("a","b")))
  }
}
