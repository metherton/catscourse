package utilsScala2

import cats.Functor
import cats.effect.MonadCancel
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration
package object general {

  implicit class DebugWrapper[F[_], A](fa: F[A]) {
    def debug1(implicit functor: Functor[F]): F[A] = fa.map { a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }


  }

  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

}
