package day14

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day14Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input = """
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
    """.trim
    val actual = day14.part1.solve(input.split("\n").toList)
    val expected = 165

    actual shouldBe expected
  }

}
