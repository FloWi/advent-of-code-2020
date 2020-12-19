package day18

import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day18Part1Test extends AnyFunSuite with Matchers {

  test("examples") {
    val examples = List(
      ("1 + 2 * 3 + 4 * 5 + 6", 71),
      ("1 + (2 * 3) + (4 * (5 + 6))", 51),
      ("2 * 3 + (4 * 5)", 26),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
    )

    examples.foreach { case (expression, expected) =>
      val actual = day18.part1.solve(List(expression))

      actual shouldBe expected
    }

  }
}

class Day18Part2Test extends AnyFunSuite with Matchers {

  test("examples") {
    val examples = List(
      ("1 + 2 * 3 + 4 * 5 + 6", 231),
      ("1 + (2 * 3) + (4 * (5 + 6))", 51),
      ("2 * 3 + (4 * 5)", 46),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)
    )

    examples.foreach { case (expression, expected) =>
      val actual = day18.part2.solve(List(expression))

      actual shouldBe expected
    }

  }
}
