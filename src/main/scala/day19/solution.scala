package day19

import cats.parse.{Parser, Parser1}
import day19.Day19._
import helper.Helper._

import scala.annotation.tailrec

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    val rules = lines.takeWhile(_.nonEmpty).map(parseRuleLine).map(r => (r.id, r)).toMap
    val messages = lines.drop(rules.size).filterNot(_.isEmpty)

    val parser = getParser(0, rules)

    val matchingMessages = messages.filter(msg => parser.parse(msg).isRight)

    matchingMessages.size
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    val originalRules = lines.takeWhile(_.nonEmpty).map(parseRuleLine).map(r => (r.id, r)).toMap

    val messages = lines.drop(originalRules.size).filterNot(_.isEmpty)

    val replacementRules = List(
      "8: 42 | 42 8",
      "11: 42 31 | 42 11 31"
    ).map(parseRuleLine)
      .map(r => (r.id, r))
      .toMap

    val updatedRules = originalRules ++ replacementRules
    val parser = getParser(0, updatedRules)

    val matchingMessages = messages.filter(msg => parser.parse(msg).isRight)

    matchingMessages.size
  }
}

object Day19 {

  sealed trait Rule {
    def id: Int
  }
  case class SingleCharacterRule(id: Int, char: Char) extends Rule
  case class OneAfterTheOtherRule(id: Int, rules: List[Int]) extends Rule
  case class OrRule(id: Int, ruleRefList1: List[Int], ruleRefList2: List[Int]) extends Rule

  def parseRuleLine(line: String): Rule = {

    val List(id, rulePart) = line.split(": ").toList

    def parseRule(rulePart: String): Rule = {
      if (rulePart.contains("|")) {
        //or rule
        // e.g.
        // 1: 2 3 | 3 2
        // 28: 104 | 95
        val parts = rulePart.split(" \\| ").toList

        val List(ruleRefs1, ruleRefs2) = parts.map(_.split(" ").map(_.toInt).toList)

        OrRule(id.toInt, ruleRefs1, ruleRefs2)

      } else if (rulePart.contains("\"")) {
        //single char rule
        SingleCharacterRule(id.toInt, rulePart.replace("\"", "").head)
      } else {
        //one after the other rule
        val rules = rulePart.split(" ").map(_.toInt).toList
        OneAfterTheOtherRule(id.toInt, rules)
      }
    }

    parseRule(rulePart)
  }

  def getParser(ruleId: Int, ruleMap: Map[Int, Rule]): Parser[Unit] = {

    import cats.parse.{Parser => P}

    def parser(ruleId: Int): Parser[Unit] = {
      ruleMap(ruleId) match {
        case SingleCharacterRule(id, char) =>
          P.char(char)
        case OneAfterTheOtherRule(id, rules) =>
          chain(rules.map(id => parser(id)))

        case OrRule(id, ruleRefList1, ruleRefList2) =>
          P.oneOf(chain(ruleRefList1.map(parser)) :: chain(ruleRefList2.map(parser)) :: Nil)
      }
    }

    parser(ruleId)
  }

  def chain(rules: List[Parser[Unit]]): Parser[Unit] = {
    @tailrec
    def helper(rest: List[Parser[Unit]], result: Parser[Unit]): Parser[Unit] = {
      rest match {
        case Nil            => result
        case ::(head, tail) => helper(tail, (result ~ head).void)
      }
    }
    helper(rules, cats.parse.Parser.pure(()))
  }
}
