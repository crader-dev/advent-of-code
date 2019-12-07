package com.crader.advent.year2019

import com.crader.advent.SingleInputSolution
import com.crader.advent.util.InputFile

import scala.annotation.tailrec
import scala.util.Try


object Day1 extends SingleInputSolution[Seq[Int], Int, Int] {

  override protected def parse(inputPath: String): Try[Seq[Int]] = {
    InputFile.asCollection(inputPath)(_.toInt)(Seq)
  }

  override protected def part1(moduleMasses: Seq[Int]): Try[Int] = Try { moduleMasses.iterator.map(fuel1).sum }

  override protected def part2(moduleMasses: Seq[Int]): Try[Int] = Try { moduleMasses.iterator.map(fuel2(_, 0)).sum }

  // integer division truncates by default
  private def fuel1(mass: Int) = (mass / 3) - 2

  @tailrec
  private def fuel2(mass: Int, total: Int): Int = {
    val m = fuel1(mass)
    if (m <= 0) total else fuel2(m, total + m)
  }
}
