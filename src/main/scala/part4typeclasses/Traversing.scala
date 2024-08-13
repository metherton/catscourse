package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
      we have a List[String]
      String => Future[Int]
      we want a Future[List[Int]]
   */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
  val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // TODO 1:
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement = func(element)
      (wAccumulator, wElement).mapN(_ :+ _)
    }
  }

  // TODO 2:
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = {
    listTraverse(list)(identity) // identity is same as x => x
  }

  // TODO 3 - whats the result of
  import cats.instances.vector._
  val allPairs = listSequence(List(Vector(1,2), Vector(3,4))) // Vector[List[Int]] - all the possible 2 - pairs
  val allTriples = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))) // Vector[List[Int]] - all the possible 3 - pairs

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
  }

}
