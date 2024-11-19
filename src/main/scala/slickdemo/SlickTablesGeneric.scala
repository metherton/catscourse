package slickdemo

import slick.jdbc.PostgresProfile
import slick.lifted

import java.time.LocalDate
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class SlickTablesGeneric(val profile: PostgresProfile) {

  import profile.api._

  lazy val movieTable = TableQuery[MovieTable]
  class MovieTable(tag: Tag) extends Table[Movie](tag, Some("movies"), "Movie") {
    def id = column[Long]("movie_id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def releaseDate = column[LocalDate]("release_date")
    def lengthInMin = column[Int]("length_in_min")
    override def * = (id, name, releaseDate, lengthInMin) <> (Movie.tupled, Movie.unapply)
  }

}

object SlickTables extends SlickTablesGeneric(PostgresProfile)

import PostgresProfile.profile.api._
object Connection {
  val db = Database.forConfig("postgres")
}

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val shawshank = Movie(1L, "Shawshank Redemption", LocalDate.of(1994, 4, 2), 162)
  val godfather = Movie(2L, "The Godfather", LocalDate.of(1994, 4, 2), 162)

  def insertMovie(movie: Movie): Future[Int] = {
    val insertQuery = SlickTables.movieTable += movie
    Connection.db.run(insertQuery)
  }

  val result = insertMovie(shawshank)

  import scala.concurrent.duration._

  println(Await.result(result, 3.seconds))

  val result2 = insertMovie(godfather)

  import scala.concurrent.duration._

  println(Await.result(result, 3.seconds))


  result.onComplete {
    case Success(value) => println(s"value is $value")
    case Failure(t) => println(s"error $t")
  }

}