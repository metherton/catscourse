package part3Concurrency

import cats.effect.{IO, IOApp, Resource}
import utils._

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration._
object Resources extends IOApp.Simple {

  // use case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").debug1
    def close: IO[String] = IO(s"closing connection to $url").debug1
  }

  val aSyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open *> IO.sleep((Int.MaxValue).seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  // problem - leaking resources


   val correctASyncFetchUrl = for {
     conn <- IO(new Connection("rockthejvm.com"))
     fib <- (conn.open *> IO.sleep((Int.MaxValue).seconds)).onCancel(conn.close.void).start
     _ <- IO.sleep(1.second) *> fib.cancel
   } yield ()

  /*
   bracket pattern - someIO.bracket(useResourceCb)(releaseCb)
   bracket is equivalent to try-catchers (pure FP)
   */
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(3.second) *> fib.cancel
  } yield ()

  /*
      Exercise - read the file with the bracket pattern
      open a file with text to print all lines, 1 every 100ms and then close file
      - open a scanner
      - read the file line by line every 100millis
      - close the scanner
      - if cancelled / throws error, close the scanner
   */
  def openScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug1 >> IO.sleep(100.millis) *> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
    openScanner(path).bracket { scanner =>
        readLineByLine(scanner)
    } { scanner =>
      IO(s"closing file at $path").debug1 *> IO(scanner.close())
    }

  /**
   * Resources
   * @return
   */

  def connFromConfig(path: String): IO[Unit] =
    openScanner(path)
      .bracket { scanner =>
        // acquire a connection based on a file
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open >> IO.never
        }(conn => conn.close.void)
      } { scanner => IO(s"closing file").debug1 >>IO(scanner.close())
      }
  // nesting resources is tedious

  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close.debug1.void)
  // .. at a later part in your code
  val resourceFromUrl = for {
    fib <- connectionResource.use(conn => conn.open >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  override def run: IO[Unit] = resourceFromUrl.void
}
