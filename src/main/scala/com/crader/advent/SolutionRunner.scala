package com.crader.advent

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import scala.util.{Failure, Success, Try}

/** Runner for all Advent of Code solutions in this project.
 *
 * This was inspired by [[https://github.com/matelaszlo/advent-of-code-scala/blob/master/src/main/scala/com/lmat/adventofcode/PuzzleRunner.scala Máté László's PuzzleRunner]].
 */
object SolutionRunner extends App {
  val (day, year) = init()
  solve(day, year)

  // args could be:
  //  - none (current day & year, rounding down to 25th day)
  //  - day (given day between 1 and 25, and current year)
  //  - day and year (given day between 1 and 25, and given year)
  private def init() = {
    lazy val currentDate = LocalDate.now()
    val year = args.lift(1).fold(currentDate.getYear)(_.toInt)
    val day = args.headOption.fold(getCurrentPuzzleDay(year, currentDate))(_.toInt)

    (day, year)
  }

  private def solve(day: Int, year: Int): Unit = {
    Solutions.getSolution(day, year) match {
      case None => println(s"Day $day of $year has not been solved yet")
      case Some(solution) =>
        println(s"Running solution for Day $day of $year..")
        solution.solve(getInputPath(day, year)) match {
          case (Success(part1Result), Success(part2Result)) =>
            println(
              s"""Results:
                 |  Part 1: $part1Result
                 |  Part 2: $part2Result
                 |""".stripMargin
            )
          case (Failure(t), _) =>
            println(s"Error in Part 1:\n${t.getMessage}")
          // Only print a Part 2 error if Part 1 did not have an error
          case (_, Failure(t)) =>
            println(s"Error in Part 2:\n${t.getMessage}")
        }
    }
  }

  /** Calculate the current puzzle day of the given `puzzleYear` by getting the difference between the current date
   * and December 1st of the given `puzzleYear`.
   *
   * Examples:
   *    - Nov 30, 2019  ->  Day  1, 2019
   *    - Dec  1, 2019  ->  Day  1, 2019
   *    - Dec  5, 2019  ->  Day  5, 2019
   *    - Dec 25, 2019  ->  Day 25, 2019
   *    - Dec 26, 2019  ->  Day 25, 2019
   *    - Jan  1, 2020  ->  Day  1, 2020
   */
  private def getCurrentPuzzleDay(puzzleYear: Int, currentDate: LocalDate) = {
    (ChronoUnit.DAYS.between(LocalDate.of(puzzleYear, 12, 1), currentDate) + 1)
      .min(25)
      .max(1)
      .toInt
  }

  /**
   * Get the resource path (relative) for the input file corresponding to the given `day` and `year`.
   */
  private def getInputPath(day: Int, year: Int) = {
    s"$year/Day${"%02d".format(day)}.txt"
  }
}

object Solutions {
  // year -> day -> solution
  private val solutionMap: Map[Int, Map[Int, Solution[_, _, _, _]]] = Map(
    2019 -> Map(
      1 -> year2019.Day1
    )
  )

  /** Get the solution for the given `day` and `year`
   */
  def getSolution(day: Int, year: Int): Option[Solution[_, _, _, _]] = {
    solutionMap.get(year).flatMap(_.get(day))
  }
}
