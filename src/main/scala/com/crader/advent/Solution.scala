package com.crader.advent

import scala.util.{Failure, Try}

// TODO: This would be a great place to try out Tagless Final (replacing Try)
sealed trait Solution[I1, I2, O1, O2] {
  protected def part1(input: I1): Try[O1]
  protected def part2(input: I2): Try[O2]

  // Only a single input file is supported/needed
  def solve(inputPath: String): (Try[O1], Try[O2])
}

/** Solution that takes the same input type for both parts.
 *
 * @tparam I Input type for Part 1 & 2
 * @tparam O1 Output type for Part 1
 * @tparam O2 Output type for Part 2
 */
trait SingleInputSolution[I, O1, O2] extends Solution[I, I, O1, O2] {
  protected def parse(inputPath: String): Try[I]

  override def solve(inputPath: String): (Try[O1], Try[O2]) = {
    val input = parse(inputPath)

    (input.flatMap(part1), input.flatMap(part2))
  }
}

/** Solution that takes different input types for both parts.
 *
 * @tparam I1 Input type for Part 1
 * @tparam I2 Input type for Part 2
 * @tparam O1 Output type for Part 1
 * @tparam O2 Output type for Part 2
 */
trait MultiInputSolution[I1, I2, O1, O2] extends Solution[I1, I2, O1, O2] {
  protected def parse1(inputPath: String): Try[I1]
  protected def parse2(inputPath: String): Try[I2]

  override def solve(inputPath: String): (Try[O1], Try[O2]) = {
    val input1 = parse1(inputPath)
    val input2 = parse2(inputPath)

    (input1.flatMap(part1), input2.flatMap(part2))
  }
}
