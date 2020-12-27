package day17

import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day17Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
.#.
..#
###
    """.trim
    val actual = day17.part1.solve(input.split("\n").toList)
    val expected = 112

    actual shouldBe expected
  }

}
