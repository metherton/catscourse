package part5polymorphic

import cats.effect.{Async, Concurrent, IO, IOApp, Sync, Temporal}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import utilsScala2.general._



object PolymorphicAsync extends IOApp.Simple {


  // Async - asynchronous computations, "suspended" in F
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // funcdamental description of async computations
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[IO[Unit]]]): F[A]
    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(kb => map(pure(cb(kb)))(_ => None))
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]
    def never[A]: F[A] = async_(_ => ()) // never ending effect

  }

  val asyncIO = Async[IO] // implicit Async[IO]

  // pure, map / flatMap, raiseError, uncancelable, start, ref/deferred, sleep, delay/defer/blocking, *
  val ec = asyncIO.executionContext


  // power,: async_ + async : FFI foreign function interface
  val threadPool = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit

  val asyncMeaningOfLife: IO[Int] = IO.async_ {(cb: Callback[Int]) =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMeaningOfLifeV2: IO[Int] = asyncIO.async_ {(cb: Callback[Int]) =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] computing an async MOL")
      cb(Right(42))
    }
  } // same

  val asyncMeaningOfLifeComplex: IO[Int] = IO.async {(cb: Callback[Int]) =>
    // start computation on some other thread pool
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Cancelled!").debug1.void)) // <-- finalizer in case the computation gets cancelled
  }

  val asyncMeaningOfLifeComplexV2: IO[Int] = asyncIO.async {(cb: Callback[Int]) =>
    // start computation on some other thread pool
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] computing an async MOL")
        cb(Right(42))
      }
    }.as(Some(IO("Cancelled!").debug1.void)) // <-- finalizer in case the computation gets cancelled
  }

  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncMeaningOfLifeV3 = asyncIO.evalOn(IO(42).debug1, myExecutionContext).guarantee(IO(threadPool.shutdown()))

  // never
  val neverIO = asyncIO.never

  /*
      Exercises:
      1. Implement never and async_ in terms of the big async
      2. Tuple two effects with different requirements

   */

  def firstEffect[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def secondEffect[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  import cats.syntax.functor._ // map extensiion method
  import cats.syntax.flatMap._ // flatMap extensin method

  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] = for {
    first <- firstEffect(a)
    second <- secondEffect(a)
  } yield (first, second)

  override def run: IO[Unit] = ???
}
