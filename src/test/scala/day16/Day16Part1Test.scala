package day16

import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day16Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
    """.trim
    val actual = day16.part1.solve(input.split("\n").toList)
    val expected = 71

    actual shouldBe expected
  }

}

class Day16part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
    """.trim
    val actual = day16.part2.solve(input.split("\n").toList)
    val expected = 1716

    actual shouldBe expected
  }

  test("real world") {
    val input = Helper.source(None).getLines.toList

    val actual = day16.part2.solve(input)
    val expected = 1716

    actual shouldBe expected
  }

}
