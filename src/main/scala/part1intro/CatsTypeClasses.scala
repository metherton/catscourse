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

  def main(args: Array[String]): Unit = {
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    println(increment(List(1, 2,3)))
    println(increment(Option(1)))
    println(Await.result(Future(1), 30.second))
  }
}
