package day15

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day15Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
0,3,6
    """.trim
    val actual = day15.part1.solve(input.split("\n").toList)
    val expected = 436

    actual shouldBe expected
  }

  test("example 2") {
    val input =
      """
1,3,2
    """.trim
    val actual = day15.part1.solve(input.split("\n").toList)
    val expected = 1

    actual shouldBe expected
  }

}

class Day15Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
0,3,6
    """.trim
    val actual = day15.part2.solve(input.split("\n").toList)
    val expected = 175594

    actual shouldBe expected
  }

  test("example 2") {
    val input =
      """
1,3,2
    """.trim
    val actual = day15.part2.solve(input.split("\n").toList)
    val expected = 2578

    actual shouldBe expected
  }

}
