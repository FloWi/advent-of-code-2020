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

    val regexString = generateRegexString(0, updatedRules)

    println(s"regex: $regexString")

    messages.count(_.matches(regexString))
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

  def generateRegexString(ruleId: Int, ruleMap: Map[Int, Rule]): String = {

    val rule = ruleMap(ruleId)
    println(s"generateRegexString for rule $rule")

    rule match {
      case SingleCharacterRule(id, char)   => char.toString
      case OneAfterTheOtherRule(id, rules) => rules.map(generateRegexString(_, ruleMap)).mkString
      case OrRule(id, ruleRefs1, ruleRefs2) =>
        val isRecursive1 = ruleRefs1.contains(id)
        val isRecursive2 = ruleRefs2.contains(id)

        if (isRecursive1 || isRecursive2) {
          val (recursivePart, nonRecursivePart) = {
            if (isRecursive1 && !isRecursive2) (ruleRefs1, ruleRefs2)
            else if (!isRecursive1 && isRecursive2) (ruleRefs2, ruleRefs1)
            else throw new RuntimeException("can't generate regex with both parts recursive")
          }

          println(s"found recursion in rule $rule")

          //generate the non-recursive part first
          val nonRecursiveRegex = nonRecursivePart.map(generateRegexString(_, ruleMap)).mkString

          val recursiveRegex = recursivePart.map { i =>
            if (i == id) {
              s"($nonRecursiveRegex)*"
            } else {
              generateRegexString(i, ruleMap)
            }
          }.mkString

          s"($recursiveRegex|$nonRecursiveRegex)"
        } else {
          val rule1Regex = ruleRefs1.map(generateRegexString(_, ruleMap)).mkString
          val rule2Regex = ruleRefs2.map(generateRegexString(_, ruleMap)).mkString
          s"($rule1Regex|$rule2Regex)"
        }
    }
  }
}
