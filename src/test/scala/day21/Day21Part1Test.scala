package day21

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day21Part1Test extends AnyFunSuite with Matchers {
  val input =
    """
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
    """.trim

  test("example 1") {

    val actual = day21.part1.solve(input.split("\n").toList)
    actual shouldBe 5
  }

}

class Day21Part2Test extends AnyFunSuite with Matchers {
  val input =
    """
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
    """.trim

  test("example 1") {
    val actual = day21.part2.solve(input.split("\n").toList)

    actual shouldBe "mxmxvkd,sqjhc,fvjkl"
  }

}
