package day16

// start: 18:48
//   end: 19:54
//      =  1:06h

import helper.Helper._

object part1 {

  import Day16._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val ParseResult(rules, yourTicket, nearbyTickets, ticketValues) = parse(lines)

    val validNumbers = rules.values.flatMap(_._2).toSet
    val invalidValues = ticketValues.map { values =>
      values.filterNot(validNumbers.contains).sum
    }

    invalidValues.sum
  }
}
object part2 {

  import Day16._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val ParseResult(rules, yourTicket, nearbyTickets, ticketValues) = parse(lines)

    val validNumbers = rules.values.flatMap(_._2).toSet
    val validTickets = ticketValues.filterNot { values =>
      values.exists(v => !validNumbers.contains(v))
    }

    val okMap: Map[(Int, Int), Boolean] = 1.to(rules.size).flatMap(x => 1.to(rules.size).map(y => ((x, y), true))).toMap

    validTickets.zipWithIndex.foreach { case (ticket, i) =>
      rules.zipWithIndex.foreach { case (rule, j) =>
        val ruleRanges = rule._2._1
      // if this ticket position is invalid, mark it as invalid in the okMap
      }

    }

    def helper(ok: Map[(Int, Int), Boolean]) = {}

    ???

  }
}

object Day16 {

  case class ParseResult(
      rules: Map[String, (List[Range.Inclusive], Set[Int])],
      yourTicket: List[String],
      nearbyTickets: List[String],
      ticketValues: List[List[Int]]
  )

  def parse(lines: List[String]): ParseResult = {
    val rules: Map[String, (List[Range.Inclusive], Set[Int])] = lines
      .takeWhile(_.nonEmpty)
      .map { line =>
        // class: 1-3 or 5-7
        // row: 6-11 or 33-44
        val List(ruleId, numberRangesString) = line.split(": ").toList
        val rangeStrings = numberRangesString.split(" or ")
        val ranges = rangeStrings.toList.map { str =>
          val List(from, to) =
            str.split("-").toList.map(_.toInt)
          from.to(to)
        }
        ruleId -> (ranges, ranges.flatMap(_.toSet).toSet)
      }
      .toMap

    val yourTicket: List[String] = lines.dropWhile(line => !line.startsWith("your")).takeWhile(_.nonEmpty)

    val nearbyTickets: List[String] = lines.dropWhile(line => !line.startsWith("nearby")).drop(1)
    val ticketValues: List[List[Int]] = nearbyTickets.map(line => line.split(",").map(_.toInt).toList)

    ParseResult(rules, yourTicket, nearbyTickets, ticketValues)
  }

}
