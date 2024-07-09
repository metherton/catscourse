package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 - how would you create all combinations of (number, char)
  val combinations = for {
    i <- numbersList
    c <- charsList
  } yield (i, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.2: how do you create the combinations of (number, char)
  val combOpt = for {
    no <- numberOption
    co <- charOption
  } yield (no, co)

  // future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('z')

  val combFut = for {
    nf <- numberFuture
    cf <- charFuture
  } yield (nf, cf)

  /*
    Pattern
    - wrapping a value into a M value
    - the flatMap mechanism

    MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO: Implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = {
      flatMap(ma)(a => pure(f(a)))
    }
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) === Some(4)

  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  // TODO 2: use a monad of a future
  import cats.instances.future._
  val futureMonad = Monad[Future] // require an explicit execution context
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 5)) // future that will end up with a success(208)


  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = {
    numbers.flatMap(i => chars.map(c => (i, c)))
  }

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = {
    number.flatMap(i => char.map(c => (i, c)))
  }

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = {
    number.flatMap(i => char.map(c => (i, c)))
  }

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))
  }

  // extension methods - weirder imports - pure and flatMap
  import cats.syntax.applicative._ // pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be use => Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap._

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extend Functors
  import cats.syntax.functor._
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  val oneOptionMapped2 = oneOption.map(_ + 2)



  // for comprehensions
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for comprehensions
  def getPairs2[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    //monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  }

  def main(args: Array[String]): Unit = {
    println(combinations)
    println(aTransformedList)
    println(getPairs(numbersList,charsList))
    println(getPairs(numberOption,charOption))
    println(getPairs(numberFuture,charFuture).foreach(println))
  }
}
