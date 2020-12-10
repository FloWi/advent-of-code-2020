package day10

// part 1
// start: 21:39
//   end: 21:18

// 39min
// wouldn't have been able to solve it that quickly without the help of Jonathan Paulson's solution.
// https://www.youtube.com/watch?v=cE88K2kFZn0
// need to fight lack of sleep

import helper.Helper._

object day10 {}

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(_.toInt)

    val solutionPart1 = solvePart1(input)
    val solutionPart2 = solvePart2(input)

    println(
      s"Solution for ${getCallingMainClass} is: $solutionPart1 --> ${solutionPart1._1 * solutionPart1._2}"
    )
    println(s"Solution for ${getCallingMainClass} part 2 is: $solutionPart2")
  }

  def solvePart1(vec: Vector[Int]): (Int, Int) = {
    val sorted = vec.sorted
    val workingVector = Vector(0) ++ sorted ++ Vector(sorted.max + 3)

    workingVector
      .sliding(size = 2, step = 1)
      .foldLeft((0, 0)) { case ((acc1, acc3), Vector(lower, higher)) =>
        //println(s""" acc1: $acc1 acc3: $acc3 lower: $lower higher: $higher""")
        val diff = higher - lower
        if (diff == 1) (acc1 + 1, acc3)
        else if (diff == 3) (acc1, acc3 + 1)
        else (acc1, acc3)
      }

  }

  def solvePart2(vec: Vector[Int]): Long = {
    val sorted = vec.sorted
    val workingVector = Vector(0) ++ sorted ++ Vector(sorted.max + 3)

    //this can somehow be made tail recursive by reversing the direction of the recursion.
    //But I can't think of a way to do so right now.

    val dp = collection.mutable.Map.empty[Int, Long]
    def helper(i: Int): Long = {
      if (i == workingVector.size - 1) {
        //println(s"reached end: i = $i")
        1L
      } else if (dp.contains(i)) {
        dp(i)
      } else {
        val res = (i + 1)
          .until(workingVector.size)
          .toList
          .map { j =>
            //println(s"  i=$i; j=$j")
            if (workingVector(j) - workingVector(i) <= 3) helper(j) else 0
          }
          .sum
        dp.update(i, res)
        //println(s"i=$i; res: $res")
        res
      }
    }
    helper(0)
  }
}
