package com.crader.advent

trait Solution[R1, R2] {
  def solve(inputPath: String): (R1, R2)
}
