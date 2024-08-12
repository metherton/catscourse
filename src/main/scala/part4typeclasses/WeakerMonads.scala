package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // hint: Apply extends Functor
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      // access to flatMap and map
      flatMap(wa)(a => map(wf)(f => f(a)))
   //          |  |        /   \     \/
   //          |  |   M[A=>B]  A=>B  B
   //          |  |   \_____   _____/
   //        M[A] A =>      M[B]
    }
  }
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    // TODO: Implement this
    override def map[A, B](ma: M[A])(f: A => B): M[B] = {
      flatMap(ma)(a => pure(f(a)))
    }
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)
  def main(args: Array[String]): Unit = {

  }

}
