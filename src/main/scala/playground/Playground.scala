package playground

import cats.Eval
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Playground {

  val meaningOfLife = Eval.later {
    println("Learning Cats: computing abstractions and the meaning of life...")
    42
  }



  case class UpdateRequest(anonymousDialing: Option[Boolean])
  case class WorkableUpdateModel(anonymousDialing: Option[Boolean])
  case class WorkerVo(anonymousDialing: Boolean)
  def main(args: Array[String]): Unit = {


    def bla(): List[String] = {
      Thread.sleep(2000)
      List("Martin", "Erna")
    }
    def getNames(): Future[List[String]] = {
      import scala.concurrent.ExecutionContext.Implicits.global
      Future.successful(bla())
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    val re = for {
      res <- getNames().map(_.map(n => {
        println(s"names are $n")
        n
      }))
    } yield res
//    val re = (for {
//      res <- getNames()
//    } yield res).map(bla => {
//      println(s"names are $bla")
//    })


    val myFile = "04.txt"
//    println(myFile.split(".")(1))


    val births = List(("martin", "sheffield"), ("erna", "emmeloord"))

    println(births.unzip)

    val one = Option(1)
    val two = Option(2)

    val result = one.map(_ + 3)
    println(result)
    val r2 = for {
      o <- one
      t <- two
    } yield o + t

    val r3 = one.flatMap(o => two.map(t => o + t))

    println(meaningOfLife.value)


    case class Person(name: String, address: String)
    val mart = Person("martin", "sheffield")
    val dyls = Person("dylan", "marknesse")

    val people = List(mart, dyls)

    val result1: IO[List[Person]] = IO.delay {
      println(s"returning $people")
      people
    }


    //println(s"result1: ${result1.unsafeRunSync()}")


//    val up1 = UpdateRequest(Some(false))
//    val wk1 = WorkerVo(false)
//
//    val wum1 = WorkableUpdateModel(up1.anonymousDialing.filterNot(_.equals(wk1.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum1)
//
//    val up2 = UpdateRequest(Some(false))
//    val wk2 = WorkerVo(true)
//
//    val wum2 = WorkableUpdateModel(up2.anonymousDialing.filterNot(_.equals(wk2.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum2)
//
//    val up3 = UpdateRequest(Some(true))
//    val wk3 = WorkerVo(false)
//
//    val wum3 = WorkableUpdateModel(up3.anonymousDialing.filterNot(_.equals(wk3.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum3)
//
//    val up4 = UpdateRequest(Some(true))
//    val wk4 = WorkerVo(true)
//
//    val wum4 = WorkableUpdateModel(up4.anonymousDialing.filterNot(_.equals(wk4.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum4)
//
//
//    val up5 = UpdateRequest(None)
//    val wk5 = WorkerVo(false)
//
//    val wum5 = WorkableUpdateModel(up5.anonymousDialing.filterNot(_.equals(wk5.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum5)
//
//    val up6 = UpdateRequest(None)
//    val wk6 = WorkerVo(true)
//
//    val wum6 = WorkableUpdateModel(up6.anonymousDialing.filterNot(_.equals(wk6.anonymousDialing)).map(res => if (res) true else false ))
//    println(wum6)


    val up1 = UpdateRequest(Some(false))
    val wk1 = WorkerVo(false)

    val wum1 = WorkableUpdateModel(up1.anonymousDialing.filterNot(_.equals(wk1.anonymousDialing)))
    println(wum1)

    val up2 = UpdateRequest(Some(false))
    val wk2 = WorkerVo(true)

    val wum2 = WorkableUpdateModel(up2.anonymousDialing.filterNot(_.equals(wk2.anonymousDialing)))
    println(wum2)

    val up3 = UpdateRequest(Some(true))
    val wk3 = WorkerVo(false)

    val wum3 = WorkableUpdateModel(up3.anonymousDialing.filterNot(_.equals(wk3.anonymousDialing)))
    println(wum3)

    val up4 = UpdateRequest(Some(true))
    val wk4 = WorkerVo(true)

    val wum4 = WorkableUpdateModel(up4.anonymousDialing.filterNot(_.equals(wk4.anonymousDialing)))
    println(wum4)


    val up5 = UpdateRequest(None)
    val wk5 = WorkerVo(false)

    val wum5 = WorkableUpdateModel(up5.anonymousDialing.filterNot(_.equals(wk5.anonymousDialing)))
    println(wum5)

    val up6 = UpdateRequest(None)
    val wk6 = WorkerVo(true)

    val wum6 = WorkableUpdateModel(up6.anonymousDialing.filterNot(_.equals(wk6.anonymousDialing)))
    println(wum6)




  }
}
