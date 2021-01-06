package day23

import day23.Day23._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day23Part1Test extends AnyFunSuite with Matchers {

  test("example 1 - parse") {
    val actual = parse("389125467")
    actual.allElementsHeadToTail.map(_.value) shouldBe List(3, 8, 9, 1, 2, 5, 4, 6, 7)
  }

  test("example 1 - play round 1") {
    val state = parse("389125467")
    val (nextCurrentCup, newState) = playRound(currentCup = 3, cups = state)
    nextCurrentCup shouldBe 2
    ringAssert(newState.allElementsHeadToTail.map(_.value), List(3, 2, 8, 9, 1, 5, 4, 6, 7))
  }

  test("example 1 - play round 2") {
    val (nextCurrentCup, newState) = playRound(currentCup = 2, cups = CircularList.create(List(3, 2, 8, 9, 1, 5, 4, 6, 7)))
    nextCurrentCup shouldBe 5
    ringAssert(newState.allElementsHeadToTail.map(_.value), List(3, 2, 5, 4, 6, 7, 8, 9, 1))
  }

  test("example 1 - play round 3") {
    val (nextCurrentCup, newState) = playRound(currentCup = 5, cups = CircularList.create(List(3, 2, 5, 4, 6, 7, 8, 9, 1)))
    nextCurrentCup shouldBe 8
    ringAssert(newState.allElementsHeadToTail.map(_.value), List(7, 2, 5, 8, 9, 1, 3, 4, 6))
  }

  test("example 1 - play round 4") {
    val (currentCup, input) = parseGameoutput("7  2  5 (8) 9  1  3  4  6")
    val (nextCurrentCup, newState) = playRound(currentCup = currentCup, cups = CircularList.create(input))

    val (expectedNextCurrentCup, expectedCups) = parseGameoutput("3  2  5  8 (4) 6  7  9  1")
    nextCurrentCup shouldBe expectedNextCurrentCup
    ringAssert(newState.allElementsHeadToTail.map(_.value), expectedCups)
  }

  test("example 1 - play round 5") {
    /*
=======================================
cups:  8 (4) 6  9  1  3  7  2  5
pick up: 6, 9, 1
destination: 2
new cups: List(8, 4, 3, 7, 2, 6, 9, 1, 5)
nextCurrentCup: 3
     */
    val (currentCup, input) = parseGameoutput("3  2  5  8 (4) 6  7  9  1")
    val (expectedNextCurrentCup, expectedCups) = parseGameoutput("9  2  5  8  4 (1) 3  6  7")

    val (nextCurrentCup, newState) = playRound(currentCup = currentCup, cups = CircularList.create(input))

    nextCurrentCup shouldBe expectedNextCurrentCup
    ringAssert(newState.allElementsHeadToTail.map(_.value), expectedCups)
  }

  test("example 1 - play whole game") {
    val actual = part1.solve("389125467")
    actual shouldBe "67384529"
  }

  test("compare example game states") {

    val lines = Source.fromURL(getClass.getResource("/day23-expectedGameoutput.txt")).getLines.toList
    val expectedGameStates = lines
      .filter(_.startsWith("cups: "))
      .map(parseGameoutput)

    val result = debugPlay(parse("389125467"), 10)
    val actual = result

    actual.size shouldBe expectedGameStates.size

    actual.zip(expectedGameStates).zipWithIndex.foreach { case (((actualCurrentCup, actualCups), (expectedCurrentCup, expectedCups)), index) =>
      println(s"\n\nmove ${index + 1}")
      println(s"  expectedCurrentCup: $expectedCurrentCup")
      println(s"  actualCurrentCup: $actualCurrentCup")
      println(s"  expected: $expectedCups")
      println(s"    actual: $actualCups")
      actualCurrentCup shouldBe expectedCurrentCup
      ringAssert(actualCups, actualCups)
    }
  }

  def ringAssert(actual: List[Int], expected: List[Int]): Assertion = {

    putThisCupFirst(expected.head, actual) shouldBe expected
  }

  def putThisCupFirst(cupValue: Int, cups: List[Int]): List[Int] = {
    //    Cups:    :List(3, 4, 6, 7, 2, 5, 8, 9, 1)
    //    cupValue: 7             ^
    //    Expected :List(7, 2, 5, 8, 9, 1, 3, 4, 6)

    val indexOfFirst = cups.indexOf(cupValue)

    cups.drop(indexOfFirst) ++ cups.take(indexOfFirst)

    val c = indexOfFirst % cups.length
    cups.drop(c) ++ cups.take(c)
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

class Day23Part2Test extends AnyFunSuite with Matchers {
  test("example 1 - play whole game") {
    val actual = part2.solve("389125467")
    actual shouldBe 149245887792L
  }

  test("my input - play whole game") {
    val actual = part2.solve("784235916")
    actual shouldBe 418819514477L
  }
}
