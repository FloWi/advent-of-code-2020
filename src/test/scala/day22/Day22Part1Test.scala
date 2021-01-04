package day22

import day22.Day22._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day22Part1Test extends AnyFunSuite with Matchers {
  import day22.part1._
  val input =
    """
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
    """.trim

  test("example 1 - parse initial state") {

    val initial = parse(input)
    initial.round shouldBe 0
    initial.p1Cards.toList shouldBe List(9, 2, 6, 3, 1)
    initial.p2Cards.toList shouldBe List(5, 8, 4, 7, 10)

  }

  test("example 1 - after one round") {

    val initial = parse(input)

    val round1 = playRound(initial)
    round1.round shouldBe 1
    round1.p1Cards.toList shouldBe List(2, 6, 3, 1, 9, 5)
    round1.p2Cards.toList shouldBe List(8, 4, 7, 10)
  }

  test("example 1 - play whole game") {

    val initial = parse(input)

    val finalState = play(initial)
    finalState.round shouldBe 29
    finalState.p1Cards.toList shouldBe List()
    finalState.p2Cards.toList shouldBe List(3, 2, 10, 6, 8, 5, 9, 4, 7, 1)
  }

  test("example 1 - final score") {

    val initial = parse(input)

    val finalState = play(initial)
    findScoreOfWinningDeck(finalState) shouldBe 306
  }

}

class Day22Part2Test extends AnyFunSuite with Matchers {
  import day22.part2._
  val input =
    """
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
    """.trim

  test("example 1 - parse initial state") {

    val initial = parse(input)
    initial.round shouldBe 0
    initial.p1Cards.toList shouldBe List(9, 2, 6, 3, 1)
    initial.p2Cards.toList shouldBe List(5, 8, 4, 7, 10)

  }

  test("example 1 - play") {

    val initial = parse(input)

    val finalResult = play(initial)

    finalResult.gameState.p1Cards.toList shouldBe List()
    finalResult.gameState.p2Cards.toList shouldBe List(7, 5, 6, 2, 4, 1, 10, 8, 9, 3)
  }

  test("example with infinite loop") {
    val input = """
Player 1:
43
19

Player 2:
2
29
14
    """.trim

    val initial = parse(input)

    val round1 = play(initial)

    round1.gameState.p1Cards.toList shouldBe List(43, 19)
    round1.gameState.p2Cards.toList shouldBe List(2, 29, 14)

  }

}
