package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]

  val listOfOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfOptions
  } yield (number, char)

  // either transformer
  import cats.data.EitherT
  import cats.instances.future._
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something wrong"), Right(43), Right(2)))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))// wrap over Future(Right(45))   EitherT(Future[Either[String, Int]](Right(45)))

  /*
      TODO: exercise
      We have a multi machine cluster for your business which will receive a traffic surge following a media appearance
      We measure bandwidth in units
      We want to allocate TWO of our servers to cope with the traffic spike
      We know the current capacity of each server and we know we'll hold the traffic if the sum of bandwidths is > 250
   */
  val bandwidths = Map("server1.rockthejvm.com" -> 50, "server2.rockthejvm.com" -> 300, "server3.rockthejvm.com" -> 170 , "server4.rockthejvm.com" -> 210)

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future("Server unreachable"))//EitherT(Future[Either[String, Int]](Left("Server unreachable")))
    case Some(b) => EitherT.right(Future(b)) //EitherT(Future[Either[String, Int]](Right(b)))
  }

  // TODO 1:
  /*
     hints; call getBandwidth twice and combine results
   */

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      band1 <- getBandwidth(s1)
      band2 <- getBandwidth(s2)
    } yield band1 + band2 > 250
  }

  // TODO: 2
  /*
    hints : call canWithstandSurge + transform
   */
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = { //e.g s1 and s2 can cope with traffic spike .. or Left(why servers can't handle the surge"
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"servers $s1 and $s2 cannot cope with the incoming spike : $reason")
      case Right(false) => Left(s"servers $s1 and $s2 cannot cope with the incoming spike : not enough total bandwidth")
      case Right(true) => Right(s"servers $s1 and $s2 can cope with the incoming spike no problem")
    }
  }


  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    val resultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server4.rockthejvm.com").value
    resultFuture.foreach(println)
  }

}
