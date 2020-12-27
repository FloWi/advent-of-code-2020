package day19

import day19.Day19._
import fastparse.parse
import helper.Helper._

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    val rules = lines.takeWhile(_.nonEmpty).map(parseRuleLine).map(r => (r.id, r)).toMap
    val messages = lines.drop(rules.size).filterNot(_.isEmpty)

    val regexString = generateRegexString(0, rules)

    println(s"regex: $regexString")

    messages.count(_.matches(regexString))
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

    val replacementRules = List(
      "8: 42 | 42 8",
      "11: 42 31 | 42 11 31"
    ).map(parseRuleLine)
      .map(r => (r.id, r))
      .toMap

    val updatedRules = originalRules ++ replacementRules

    println("================ updated rules ================")
    updatedRules.toList.sortBy(_._1).foreach(println)

    val messages = lines.drop(updatedRules.size).filterNot(_.isEmpty)

    val regexString = generateRegexString(0, updatedRules, isPart2 = true)

    println(s"regex: $regexString")

    val matchingMessages = messages.filter(_.matches(regexString))

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

  def generateRegexString(ruleId: Int, ruleMap: Map[Int, Rule], isPart2: Boolean = false): String = {

    val rule = ruleMap(ruleId)
    println(s"generateRegexString for rule $rule")

    rule match {
      case SingleCharacterRule(id, char)   => char.toString
      case OneAfterTheOtherRule(id, rules) => rules.map(generateRegexString(_, ruleMap, isPart2)).mkString
      case OrRule(id, ruleRefs1, ruleRefs2) =>
        if (ruleId == 8 && isPart2) {
          generateRegexString(42, ruleMap, isPart2) + "+"
        } else if (ruleId == 11 && isPart2) {
          //11: 42 31 | 42 11 31
          val r42 = generateRegexString(42, ruleMap, isPart2)
          val r31 = generateRegexString(31, ruleMap, isPart2)
          val r11 = 1
            .to(40)
            .map(n => r42 * n + r31 * n)
            .mkString("|")

          println(s"rule 11: ${r11.take(100)}")
          r11
        } else {

          val rule1Regex = ruleRefs1.map(generateRegexString(_, ruleMap, isPart2)).mkString
          val rule2Regex = ruleRefs2.map(generateRegexString(_, ruleMap, isPart2)).mkString
          s"($rule1Regex|$rule2Regex)"
        }
    }
  }
}
