package day14

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day14Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
    """.trim
    val actual = day14.part2.solve(input.split("\n").toList)
    val expected = 208

    actual shouldBe expected
  }

}
