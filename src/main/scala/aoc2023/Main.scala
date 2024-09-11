package aoc2023



object Main extends App {

  import Puzzles._
  puzzles.foreach(test => {
    test.run()
  })
}