package part2Effects

import java.time.InstantSource.system
import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency - can replace an expression with its value
  // as many times as we want without changing behaviour


  // example : print to the console
  val printSomething: Unit = println("cats effect")
  val printSomething_v2: Unit = () // not the same

  // example : change a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1)
  val changingVar_v2: Unit = () // not the same



  // side effects are inevitable for useful programs

  // effect
  /*
      Effect types
      Properties
      - type signature describes the type of calculation that will be performed
      - type signatures describes the VALUE that will be calculated
      - when side effects are needed, effect construction is separate from effect execution
   */

   /*
     example : Option
     - describes a possibly absent value
     - computes a value of type A, if it exists
     - side effects are not needed
    */
   val anOption: Option[Int] = Option(42)


  /*
     example : Future is NOT an effect type
      - describes an asynchronous computation
      - computes a value of type  A, if its succesful
      - side effect is required (allocating/scheduling a thread) - execution is NOT separate from construction
   */

  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
      example : MyIO data type from the Monad lesson - it IS an effect type
      - describes any computation that might produce side effects
      - calculates a value of type A if its successful
      - side effects are required for the evaluation of () => A
        - YES, the creation of MyIO does NOT produce the side effects on construction
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())

  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something")
    42
  })

  /*

      Exercises
      1. An IO (use MyIO) which returns the current time of the system
      2. An IO which measures the duration of a computation (i.e an IO unsafeRun) (hint: use ex 1)
      3. An IO which prints something to the console
      4. An IO which reads something from the console,i.e stdin


   */

  // 1
  def clock: MyIO[Long] = MyIO(() => system.millis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _ <- computation
    finishTime <- clock
  } yield finishTime - startTime

  /*
      clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

      clock.map(finishTime => finishTime - startTime) = MYIO(() => clock.unsafeRun() - startTime)
                              finishTime - startTime) = MYIO(() => system.millis - startTime)

      => clock.flatMap(startTime => computation.flatMap(_ => MYIO(() => system.millis - startTime)))

      computation.flatMap(lambda) = MyIO(() => lambda(computation.unsafeRun())

      computation.flatMap(lambda) = MyIO(() => lambda(___COMP___).unsafeRun())
                                  = MyIO(() => MyIO(() => system.millis - startTime)).unsafeRun())
                                  = MyIO(() => system.millis_after_computation - startTime)


      => clock.flatMap(startTime => MyIO(() => system.millis_after_computation - startTime))
      MyIO(() => lamda(clock.unsafeRun()).unsafeRun() // lamda is startTime => MyIO(() => system.millis_after_computation - startTime)
      MyIO(() => lamda(clock.unsafeRun()).unsafeRun()
      MyIO(() => lamda(system.millis).unsafeRun()
      MyIO(() => MyIO(() => system.millis_after_computation - startTime).unsafeRun()
      MyIO(() => MyIO(() => system.millis_after_computation - system.millis()).unsafeRun()

      MyIO(() => system.millis_after_computation - system.millis_at_start()

   */


  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  // 3

  def putStrLn(line: String): MyIO[Unit] =
    MyIO(() => println(line))

  // 4
  val read: MyIO[String] = MyIO(() => StdIn.readLine())



  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()
    program.unsafeRun()
  }

  /*
      line1 = read from console
      line2 = read from console
      println( line1 + line 2 )

   */

  def main(args: Array[String]): Unit = {
    //testTimeIO()
    testConsole()
  }
}
