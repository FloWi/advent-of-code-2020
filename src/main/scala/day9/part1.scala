package day9

// part 1
// start: 13:40
//   end: 13:51

// start: 20:13
//   end: 20:26

// 11 + 13 = 24min

import helper.Helper._

object day9 {}

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(_.toLong)

    val solutionPart1 = solvePart1(input, 25).get
    val solutionPart2 = solvePart2(input, solutionPart1).get

    println(
      s"Solution for ${getCallingMainClass} is: $solutionPart1"
    )
    println(
      s"Solution for day9 part2 is: $solutionPart2"
    )
  }

  def solvePart1(vec: Vector[Long], preambleLength: Int): Option[Long] = {
    // The first step of attacking the weakness in the XMAS data is to find the first number in the list (after the preamble)
    // which is not the sum of two of the 25 numbers before it. What is the first number that does not have this property?
    vec
      .sliding(preambleLength + 1, 1)
      .find { values =>
        val valueToCheck = values.last
        val preamble = values.dropRight(1)

        !preamble.combinations(2).exists(_.sum == valueToCheck)
      }
      .map(_.last)
  }

  def solvePart2(vec: Vector[Long], solutionPart1: Long): Option[Long] = {

    // find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.

    val starts = vec.indices.toList.dropRight(1)
    val ends = 1.to(vec.size).toList

    starts
      .flatMap(s => ends.map(e => (s, e)))
      .find { case (s, e) => vec.slice(s, e).sum == solutionPart1 }
      .map { case (start, end) =>
        val slice = vec.slice(start, end)
        slice.min + slice.max
      }
  }

}
