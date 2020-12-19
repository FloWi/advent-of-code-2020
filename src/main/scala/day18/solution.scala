package day18

import fastparse.parse
import helper.Helper._

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    lines.map(parseExpression).sum
  }

  def parseExpression(line: String): Long = {
    println(s"parsing '$line'")
    parse(line, expr(_)).get.value
  }

  import fastparse._, SingleLineWhitespace._
  def number[_: P]: P[Long] = P(CharIn("0-9").rep(1).!.map(_.toLong))
  def parens[_: P]: P[Long] = P("(" ~/ operation ~ ")")
  def factor[_: P]: P[Long] = P(number | parens)

  def operation[_: P]: P[Long] = P(factor ~ (CharIn("*/+\\-").! ~/ factor).rep).map(eval)

  def eval(x: (Long, Seq[(String, Long)])): Long = {

    println(s"operation: $x")
    val (value, operations) = x
    operations.toList.foldLeft(value) { case (acc, (operator, operand)) =>
      operator match {
        case "+" => acc + operand
        case "-" => acc - operand
        case "*" => acc * operand
        case "/" => acc / operand
      }
    }
  }
  def expr[_: P]: P[Long] = P(operation ~ End)
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    lines.map(parseExpression).sum
  }

  def parseExpression(line: String): Long = {
    println(s"parsing '$line'")
    parse(line, expr(_)).get.value
  }

  import fastparse._, SingleLineWhitespace._

  def expr[_: P]: P[Long] = P(otherOperation ~ End)
  def otherOperation[_: P]: P[Long] = P(priorityOperation ~ (CharIn("*/").! ~/ priorityOperation).rep).map(eval)
  def priorityOperation[_: P]: P[Long] = P(factor ~ (CharIn("+\\-").! ~/ factor).rep).map(eval)

  def factor[_: P]: P[Long] = P(number | parens)
  def number[_: P]: P[Long] = P(CharIn("0-9").rep(1).!.map(_.toLong))
  def parens[_: P]: P[Long] = P("(" ~/ otherOperation ~ ")")

  def eval(x: (Long, Seq[(String, Long)])): Long = {
    println(s"operation: $x")

    val (value, operations) = x
    operations.toList.foldLeft(value) { case (acc, (operator, operand)) =>
      operator match {
        case "+" => acc + operand
        case "-" => acc - operand
        case "*" => acc * operand
        case "/" => acc / operand
      }
    }
  }

}
