package part2Effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(string: String): Future[Int] =  Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")
  def clunkyFutures(): Unit = {

    val futures: List[Future[Int]] = workLoad.map(heavyComputation)

    // Future[List[Int]] would be hard to obtain

    futures.foreach(_.foreach(println))
  }

  // traverse
  import cats.Traverse
  import cats.instances.list._

  val listTraverse = Traverse[List]
  def traverseFutures(): Unit = {

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)

    // this stores ALL the results
    singleFuture.foreach(println)
  }

  import utils._

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug1

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO) // computeAsIO(element) = IO[Int]

  // parallel traversal
  import cats.syntax.parallel._ // parTraverse is an extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)


  /**
   * Exercises
   * @return
   */
// HINT:  use the Traverse API

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    listTraverse.traverse(listOfIOs)(x => x) // lambda(elem) => IO[A]
  }

  // HARD version
  def sequenceV2[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = {
    Traverse[F].traverse(listOfIOs)(x => x) // lambda(elem) => IO[A]
  }

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    listOfIOs.parTraverse(x => x) // lambda(elem) => IO[A]
  }

  // HARD version
  def parSequenceV2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = {
    wrapperOfIOs.parTraverse(x => x)
  }

  // existing sequence API
  val singleIOv2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIOV2: IO[List[Int]] = parSequence(ios)  // from the exercise
  val parallelSingleIOV3: IO[List[Int]] = ios.parSequence // extension method


  override def run: IO[Unit] = {
    //singleIO.map(_.sum).debug1.void
    //parallelSingleIO.map(_.sum).debug1.void
    parallelSingleIOV3.map(_.sum).debug1.void

  }
}
