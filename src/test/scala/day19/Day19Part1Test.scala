package day19

import day19.Day19._
import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day19Part1Test extends AnyFunSuite with Matchers {
  import day19.part1._
  test("example 1") {
    val input =
      """
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
    """.trim
    val actual = solve(input)
    val expected = 2

    actual shouldBe expected
  }
}

class Day19Part2Test extends AnyFunSuite with Matchers {
  val input = Helper.source(Some("src/main/resources/day19-example-part2.txt")).mkString.trim

  test("example 1") {

    val actual = part2.solve(input)
    val expected = 12

    actual shouldBe expected
  }

  test("rule expansion part1") {

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)

    val expanded = expand(ruleMap)

    expanded.size shouldBe rules.size

    println("========================")
    println("rule #8:")
    println(expanded(8))

    println("========================")
    println("rule #31:")
    println(expanded(31))

    println("========================")
    println("rule #42:")
    println(expanded(42))

    println("========================")
    println("rule #0:")
    println(expanded(0))
    val pattern = expanded(0).r
    val validMessages = messages.filter(pattern.matches)
    println(s"${validMessages.size} out of ${messages.size} are valid")
  }

  test("rule expansion part2") {
    val input = Helper.source(Some("src/main/resources/day19.txt")).mkString.trim

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)
      .updated(8, "42 | 42 8")
      .updated(11, "42 31 | 42 11 31")

    val expanded = expand(ruleMap)
    val broken = expanded.filter(_._2.contains("{("))
    println("broken rules:")
    broken.toList.sorted.foreach(println)

    expanded.size shouldBe rules.size

    println("========================")
    println("rule #8:")
    println(expanded(8))

    println("========================")
    println("rule #11:")
    println(expanded(11))

    println("========================")
    println("rule #31:")
    println(expanded(31))

    println("========================")
    println("rule #42:")
    println(expanded(42))

    println("========================")
    println("rule #0:")
    println(expanded(0))
    val pattern = expanded(0).r
    val validMessages = messages.filter(pattern.matches)
    println(s"${validMessages.size} out of ${messages.size} are valid")
  }

  test("rule simplififaction") {

    simplifyOrRule("(a|b)a") shouldBe "aa|ba"
    simplifyOrRule("a(a|b)") shouldBe "aa|ab"
    simplifyOrRule("(a|b)") shouldBe "a|b"
    simplifyOrRule("ab|(a|b)a|c") shouldBe "ab|aa|ba|c"
    simplifyOrRule("(a|b)(a|b)") shouldBe "aa|ab|ba|bb"
  }

  test("braced or term") {
    import cats.parse.{Parser => P}
    val word = P.charsWhile1(_.isLetter)

    val orTerm = P.repSep(word, 2, P.char('|'))
    val bracedOrTerm = orTerm.between(P.char('('), P.char(')'))

    val rule = "(a|b)"
    val actual = bracedOrTerm.parse(rule)
    actual.right.get._2 shouldBe List("a", "b")

  }
  test("braced or term 2") {
    import cats.parse.{Parser => P}
    val word = P.charsWhile1(_.isLetter)

    val orTerm = P.repSep(word, 2, P.char('|'))
    val bracedOrTerm = orTerm.between(P.char('('), P.char(')'))

    val bracedOrTermWithoutBraces = bracedOrTerm.map { terms =>
      println("terms: ")
      println(terms)
      terms.mkString("|")
    }
    val rule = "(a|b)"
    val actual = bracedOrTermWithoutBraces.parse(rule)
    actual.right.get._2 shouldBe "a|b"
  }
}
