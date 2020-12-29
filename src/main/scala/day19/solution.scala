package day19

import day19.Day19._
import helper.Helper._

import scala.annotation.tailrec
import scala.util.Try

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): Long = {
    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)

    val expanded = expand(ruleMap)
    val pattern = expanded(0).r
    val validMessages = messages.filter(pattern.matches)
    println(s"${validMessages.size} out of ${messages.size} are valid")
    validMessages.size
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): Long = {
    val List(rulesStr, messagesStr) = input.split("\n\n").toList
    val rules = rulesStr.split("\n").toList
    val messages = messagesStr.split("\n").toList

    val ruleMap = parseRuleMap(rules)
      .updated(8, "42 | 42 8")
      .updated(11, "42 31 | 42 11 31")

    val expanded = expand(ruleMap)
    val pattern = expanded(0).r
    val validMessages = messages.filter(pattern.matches)
    println(s"${validMessages.size} out of ${messages.size} are valid")
    validMessages.size
  }
}

object Day19 {

  def canBeExpanded(rule: String, expanded: Map[Int, String]): Boolean = {
    if (rule.contains("\"")) {
      true
    } else {
      val ruleIds = rule.split(" \\| ").flatMap(r => r.split(" ")).map(_.toInt).toSet

      ruleIds.diff(expanded.keySet).isEmpty
    }
  }

  def expandRule(rule: String, expandable: Set[Int], resolved: Map[Int, String]): String = {

    //if any of the rules contains a regex-repetition token e.g.'{2}', don't simplify the expression

    val resolvedRules = "\\d+".r
      .findAllIn(rule)
      .toList
      .map(_.toInt)
      .map(resolved.apply)

    val containsRegexRepetition = resolvedRules.exists(_.contains("{"))

    if (containsRegexRepetition) {
      "\\d+".r
        .findAllIn(rule)
        .toList
        .map(_.toInt)
        .map(resolved.apply)
        .mkString("")
    } else {

      //sort descending so 14 will be replaced before 1 and 4 only replace parts of it

      val expanded = expandable.toList.sorted.reverse
        .foldLeft(rule) { (acc, id) =>
          val replacement = resolved(id)
          val replacedString = if (replacement.contains("|")) s"(${replacement})" else replacement
          acc.replace(id.toString, replacedString)
        }
        .replaceAll("\\s+", "")

      simplifyOrRule(expanded)
    }
  }

  @tailrec
  def simplifyOrRule(rule: String): String = {

    import cats.parse.{Parser => P}

    //  (a|b)c     ==> ac|bc
    //  (a|b)      ==> a|b
    // c(a|b)      ==> ca|cb
    // ab|(a|b)a|c ==> ab|aa|ba|c
    val term = P.charsWhile1(_.isLetter)
    val orTerm = P.repSep(term, 2, P.char('|')).backtrack
    val bracedOrTerm = orTerm.between(P.char('('), P.char(')')).backtrack
    val twoBracedOrTerms = (bracedOrTerm ~ bracedOrTerm).backtrack

    val orTermWithFactorParser = (term.? ~ bracedOrTerm ~ term.?).map { case ((maybeLeft, orTerms), maybeRight) =>
      orTerms
        .map(orTerm => maybeLeft.getOrElse("") + orTerm + maybeRight.getOrElse(""))
        .mkString("|")
    }

    val twoBracedTermsParser = twoBracedOrTerms.map { case (l1, l2) =>
      val result = (for {
        t1 <- l1
        t2 <- l2
      } yield t1 + t2)

      result.mkString("|")
    }

    val parser = P.oneOf(twoBracedTermsParser :: orTermWithFactorParser :: Nil)

    val indices = 0 :: "\\|".r.findAllMatchIn(rule).map(_.start + 1).toList
    val maybeUpdatedRule = indices.flatMap { i =>
      val (first, second) = rule.splitAt(i)
      val res = parser.parse(second)
      res match {
        case Left(_) => List.empty
        case Right((rest, result)) =>
          List(first + result + rest)
      }
    }.headOption

    val result = maybeUpdatedRule.getOrElse(rule)
    if (result != rule) {
      simplifyOrRule(result)
    } else {
      result
    }

  }

  def parseRuleMap(rules: List[String]): Map[Int, String] = {
    val ruleMap = rules.map { line =>
      val List(id, ruleString) = line.split(": ").toList

      id.toInt -> ruleString
    }.toMap

    ruleMap.toList.sorted.foreach(println)

    ruleMap
  }

  def expand(ruleMap: Map[Int, String]): Map[Int, String] = {

    @tailrec
    def helper(working: Map[Int, String], resolved: Map[Int, String]): Map[Int, String] = {
      if (working.isEmpty) {
        resolved
      } else {

        val nonExpandedIds = ruleMap.keySet.diff(resolved.keySet)

        val ruleToExpand = nonExpandedIds
          .find { id =>
            val rule = ruleMap(id)
            canBeExpanded(rule, resolved)
          }
          .map(id => id -> working(id))

        ruleToExpand match {

          case Some((id, rule)) if rule.contains("\"") =>
            helper(working.removed(id), resolved.updated(id, rule.replace("\"", "")))

          case Some((id, rule)) =>
            val ruleReferences = rule.split(" \\| ").toList.flatMap(r => r.split(" ")).flatMap { tok => Try(tok.toInt).toOption.toList }.toSet
            val expandable = ruleReferences.intersect(resolved.keySet)
            val nonExpandable = ruleReferences.diff(expandable)

            if (nonExpandable.isEmpty) {
              helper(working.removed(id), resolved.updated(id, expandRule(rule, expandable, resolved)))
            } else {

              ???
            }

          case None =>
            if (nonExpandedIds.contains(8) && resolved.contains(42)) {
              // 8: 42 | 42 8
              val rule42 = resolved(42)
              val newRule8 = s"($rule42)+"
              helper(working.removed(8), resolved.updated(8, newRule8))
            } else if (nonExpandedIds.contains(11) && resolved.contains(31) && resolved.contains(42)) {

              // 11: 42 31 | 42 11 31
              val rule31 = resolved(31)
              val rule42 = resolved(42)
              val newRule11 = 1
                .to(10)
                .map { i =>
                  val regex = s"(($rule42){$i}($rule31){$i})"
                  regex
                }
                .mkString("|")

              helper(working.removed(11), resolved.updated(11, s"($newRule11)"))
            } else
              throw new RuntimeException("nothing to expand")
        }
      }
    }

    helper(ruleMap, Map.empty)

  }

}
