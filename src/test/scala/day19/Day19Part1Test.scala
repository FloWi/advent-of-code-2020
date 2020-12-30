package day19

import cats.data.NonEmptyChain
import cats.parse.Parser.{not, unit}
import cats.parse.{Parser, Parser1}
import day19.Day19._
import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

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

  import cats.parse.{Parser => P}
  def stringParser(p1: Parser1[Char], p2: Parser1[Char]): Parser1[String] = {
    (p1 ~ p2).map { case (a, b) => s"$a$b" }.backtrack
  }

  def repeatStringParser(p: Parser1[String]): Parser1[String] = {
    P.rep1(p, 1).backtrack.map(_.toList.mkString)
//    lazy val recurse: Parser1[String] = (p ~ P.defer1(recurse)).map { case (a, b) => s"$a$b" }.backtrack.orElse1(p).backtrack
//    recurse
  }

  def repeatOrParser(pStart: Parser1[String], pEnd: Parser1[String]): Parser1[String] = {
    lazy val recurse: Parser1[String] = (pStart ~ P.defer1(recurse) ~ pEnd)
      .map { case ((a, b), c) => s"$a$b$c" }
      .backtrack
      .orElse1((pStart ~ pEnd).map { case (a, b) => s"$a$b" })
      .backtrack

    recurse
  }

  def createParser(ruleId: Int, ruleMap: Map[Int, String]): Parser1[String] = {

    val ruleString = ruleMap(ruleId)

    val ruleIds = "\\d+".r.findAllMatchIn(ruleString).map(_.matched.toInt).toSet
    val isRecursive = ruleIds.contains(ruleId)

    if (isRecursive) {
      // 8: 42 | 42 8
      // 11: 42 31 | 42 11 31
      ruleString.split(ruleId.toString).toList match {
        case Nil => ???
        case _ :: Nil =>
          val ids = ruleString.split(" \\|").head.split(" ").map(_.toInt)
          val List(first) = ids.toList.map(createParser(_, ruleMap))
          repeatStringParser(first)

        case _ :: _ :: Nil =>
          val ids = ruleString.split(" \\|").head.split(" ").map(_.toInt)
          val List(first, second) = ids.toList.map(createParser(_, ruleMap))

          repeatOrParser(first, second)
      }

    } else {

      val isTerminalParser = P.charIn('a'.to('z')).surroundedBy(P.char('"'))
      val p = isTerminalParser.parse(ruleString) match {
        case Right((_, terminalChar)) =>
          P.charIn(terminalChar).map(_.toString)
        case Left(_) =>
          ruleString.split(" \\| ").toList match {
            case Nil =>
              throw new RuntimeException(s"invalid rule: '$ruleString'")
            case first :: Nil =>
              createOneAfterTheOtherParser(first, ruleMap)
            case orLeft :: orRight :: Nil =>
              P.oneOf1(List(createOneAfterTheOtherParser(orLeft, ruleMap), createOneAfterTheOtherParser(orRight, ruleMap)))
          }
      }
      p.backtrack
    }
  }

  def createOneAfterTheOtherParser(ruleString: String, ruleMap: Map[Int, String]): Parser1[String] = {
    val parsers = ruleString.split(" ").toList.map(_.toInt).map(createParser(_, ruleMap).backtrack)
    chainParsers(parsers.map(_.backtrack)).backtrack
  }

  def chainParsers(parserList: List[Parser1[String]]): Parser1[String] = {

    @tailrec
    def helper(rest: List[Parser1[String]], acc: Parser1[String]): Parser1[String] = {
      rest match {
        case Nil            => acc
        case ::(head, tail) => helper(tail, (acc ~ head).map { case (s1, s2) => s1 + s2 }.backtrack)
      }
    }

    parserList match {
      case ::(head, tail) => helper(tail, head)
      case Nil            => P.fail
    }
  }

  test("recursive parsers") {
    // 8 : 42 | 42 8
    // 11: 42 31 | 42 11 31

    val parser42 = stringParser(P.charIn('4'), P.charIn('2'))
    val parser31 = stringParser(P.charIn('3'), P.charIn('1'))

    lazy val parser8: Parser1[String] = repeatStringParser(parser42)

    lazy val parser11: Parser1[String] = repeatOrParser(parser42, parser31)

    parser8.parse("42") shouldBe Right(("", "42"))
    parser8.parse("4242") shouldBe Right(("", "4242"))
    (parser8 ~ stringParser(P.charIn('a'), P.charIn('b'))).parse("4242ab") shouldBe Right(("", ("4242", "ab")))

    parser11.parse("4231") shouldBe Right(("", "4231"))
    parser11.parse("42423131") shouldBe Right(("", "42423131"))
    parser11.parse("424231").isLeft shouldBe true
  }

  import cats.parse.Parser
  import cats.data.Chain

  def eitherOr0[A, B](first: Parser[B], second: Parser[A]): Parser[Either[A, B]] =
    Parser.oneOf(first.map(Right(_)) :: second.map(Left(_)) :: Nil)

  def repeatUntil[A, B](r: Parser[A], until: Parser[B]): Parser[(Chain[A], B)] = {

    val recurse = Parser.defer(repeatUntil(r, until))
    eitherOr0(until, r).flatMap {
      case Left(a)  => recurse.map { case (as, b) => (as :+ a) -> b }
      case Right(b) => Parser.pure(Chain.empty[A] -> b)
    }
  }

  def repeat1Until[A, B](r: Parser[A], until: Parser[B]): Parser[(NonEmptyChain[A], B)] = {
    repeatUntil(r, until).flatMap { case (as, b) =>
      NonEmptyChain.fromChain(as) match {
        case Some(value) => P.pure((value, b))
        case None        => P.fail
      }
    }
  }

  test("applicative repeat") {

    import cats.parse.Parser
    import cats.implicits._
    val parser42: Parser[String] = stringParser(P.charIn('4'), P.charIn('2'))
    val p = parser42.replicateA(2).map(_.mkString)

    p.parse("4242") shouldBe Right(("", "4242"))
    p.parse("424242") shouldBe Right(("42", "4242"))
    p.parse("42").isLeft shouldBe true
  }

  test("recursive parsers should take only what's necessary") {
    // 8 : 42 | 42 8
    // 11: 42 31 | 42 11 31

    val parser42 = stringParser(P.charIn('4'), P.charIn('2'))
    val parser31 = stringParser(P.charIn('3'), P.charIn('1'))

    lazy val parser8: Parser1[String] = parser42.backtrack
    lazy val parser11: Parser1[String] = repeatOrParser(parser42, parser31)

    val parser = repeat1Until(parser42.backtrack, parser11)

    parser.parse("424242424242423131") shouldBe Right(("", (Chain("42", "42", "42", "42", "42"), "42423131")))
  }

  test("repeatUntil") {
    // 8 : 42 | 42 8
    // 11: 42 31 | 42 11 31

    val parser42 = stringParser(P.charIn('4'), P.charIn('2'))
    val parser31 = stringParser(P.charIn('3'), P.charIn('1'))
    lazy val parser11: Parser1[String] = repeatOrParser(parser42, parser31)

    val parser = repeat1Until(parser42, parser11).map { case (first, second) => first.append(second).toChain.toList.mkString }

    parser.parse("424242424242423131") shouldBe Right(("", "424242424242423131"))
  }

  test("fancy parser with simple input") {

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

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)
    val parser = createParser(0, ruleMap)

    val results = messages.map(parser.parseAll)
    val actualValid = results.filter(_.isRight)

    val expectedValid = List("ababbb", "abbbab")

    actualValid.flatMap(_.toOption.toList) should contain theSameElementsAs expectedValid
  }
  test("fancy parser with my own recursive input") {

    val input =
      """
0: 8 11
8: 4 | 4 8
11: 4 5 | 4 11 5
4: "a"
5: "b"

aaaaab
aaaaaabb
    """.trim

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)

//    val parser8 = createParser(8, ruleMap).backtrack
    val parser4 = createParser(4, ruleMap).backtrack

    val parser11 = createParser(11, ruleMap).backtrack

    val parser = repeat1Until(parser4, parser11)

    val results = messages.map(parser.parseAll)
    val numberValid = results.count(_.isRight)
    numberValid shouldBe 2
  }

  test("fancy parser with simple input part2 (w/o modification") {

    val input = Helper.source(Some("src/main/resources/day19-example-part2.txt")).mkString.trim

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)
    val parser = createParser(0, ruleMap)

    val results = messages.map(parser.parse)
    val numberValid = results.count(_.isRight)
    numberValid shouldBe 3

  }

  test("fancy parser with simple input part2 (with modifications)") {

    val input = Helper.source(Some("src/main/resources/day19-example-part2.txt")).mkString.trim

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
//    val messages = messagesStr.split("\n").toList
    val messages = List("bbabbbbaabaabba")

    val ruleMap = parseRuleMap(rules)
      .updated(8, "42 | 42 8")
      .updated(11, "42 31 | 42 11 31")

    val parser42 = createParser(42, ruleMap).backtrack
    val parser11 = createParser(11, ruleMap).backtrack

    val parser = repeat1Until(parser42, parser11)

    val results = messages.map(parser.parse)
    val numberValid = results.count(_.isRight)
    numberValid shouldBe 1
  }

  test("fancy parser to solve part2") {

    val input = Helper.source(Some("src/main/resources/day19.txt")).mkString.trim

    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)
      .updated(8, "42 | 42 8")
      .updated(11, "42 31 | 42 11 31")

    val parser42 = createParser(42, ruleMap).backtrack
    val parser11 = createParser(11, ruleMap).backtrack

    val parser = repeat1Until(parser42, parser11)

    val results = messages.map(parser.parse)
    val numberValid = results.count(_.isRight)
    numberValid shouldBe 400
  }
}
