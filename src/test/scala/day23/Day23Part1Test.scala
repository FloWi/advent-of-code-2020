package day23

import day23.part1._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day23Part1Test extends AnyFunSuite with Matchers {

  test("example 1 - parse") {
    val actual = parse("389125467")
    actual shouldBe List(3, 8, 9, 1, 2, 5, 4, 6, 7)
  }

  test("example 1 - play round 1") {
    val state = parse("389125467")
    val (nextCurrentCup, newState) = playRound(currentCup = 3, cups = state)
    nextCurrentCup shouldBe 2
    ringAssert(newState, List(3, 2, 8, 9, 1, 5, 4, 6, 7))
  }

  test("example 1 - play round 2") {
    val (nextCurrentCup, newState) = playRound(currentCup = 2, cups = List(3, 2, 8, 9, 1, 5, 4, 6, 7))
    nextCurrentCup shouldBe 5
    ringAssert(newState, List(3, 2, 5, 4, 6, 7, 8, 9, 1))
  }

  test("example 1 - play round 3") {
    val (nextCurrentCup, newState) = playRound(currentCup = 5, cups = List(3, 2, 5, 4, 6, 7, 8, 9, 1))
    nextCurrentCup shouldBe 8
    ringAssert(newState, List(7, 2, 5, 8, 9, 1, 3, 4, 6))
  }

  test("example 1 - play") {
    val actual = solve("389125467")
    actual shouldBe "67384529"
  }

  test("compare example game states") {

    val lines = Source.fromURL(getClass.getResource("/day23-expectedGameoutput.txt")).getLines.toList
    val expectedGameStates = lines
      .filter(_.startsWith("cups: "))
      .map(parseGameoutput)

    val result = play(parse("389125467"), 10)
    val actual = result

    actual.size shouldBe expectedGameStates.size

    actual.zip(expectedGameStates).zipWithIndex.foreach { case (((actualCurrentCup, actualCups), (expectedCurrentCup, expectedCups)), index) =>
      val actualInOrder = putThisCupFirst(expectedCups.head, actualCups)

      println(s"\n\nmove ${index + 1}")
      println(s"  expectedCurrentCup: $expectedCurrentCup")
      println(s"  actualCurrentCup: $actualCurrentCup")
      println(s"  expected: $expectedCups")
      println(s"    actual: $actualInOrder")
      actualCurrentCup shouldBe expectedCurrentCup
      ringAssert(actualInOrder, expectedCups)
    }

  }

  test("example game") {}

  def ringAssert(actual: List[Int], expected: List[Int]): Assertion = {
    putThisCupFirst(expected.head, actual) shouldBe expected
  }

  def parseGameoutput(line: String): (Int, List[Int]) = {
    val cleaned =
      line
        .replace("cups: ", "")
        .split("\\s+")
        .filterNot(_.isEmpty)

    val currentCupIndex = cleaned.indexWhere(_.contains("("))

    val cups = cleaned.map { line =>
      line
        .replace("(", "")
        .replace(")", "")
        .trim
        .toInt
    }.toList

    (cups(currentCupIndex), cups)

  }

}
