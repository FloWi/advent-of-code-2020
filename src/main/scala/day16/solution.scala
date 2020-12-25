package day16

import helper.Helper._

import scala.annotation.tailrec

object part1 {

  import Day16._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val ParseResult(rules, yourTicket, nearbyTickets, ticketValues) = parse(lines)

    val validNumbers = rules.flatMap(_._2._2).toSet
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

    val validNumbers = rules.flatMap(_._2._2).toSet
    val validTickets = ticketValues.filterNot { values =>
      values.exists(v => !validNumbers.contains(v))
    }

    val okMap: collection.mutable.HashMap[(Int, Int), Boolean] =
      collection.mutable.HashMap.from(rules.indices.flatMap(x => rules.indices.map(y => ((x, y), true))).toMap)

    validTickets.foreach { ticket =>
      ticket.zipWithIndex.foreach { case (v, i) =>
        rules.zipWithIndex.foreach { case (rule, j) =>
          val ruleRanges = rule._2._1

          val List(range1, range2) = ruleRanges
          if (!(range1.contains(v) || range2.contains(v))) {
            okMap.update((i, j), false)
          }

        // if this ticket position is invalid, mark it as invalid in the okMap

        }
      }
    }

    @tailrec
    def helper(resultMap: Map[Int, Int]): Map[Int, Int] = {
      if (resultMap.size == rules.size)
        resultMap
      else {
        val result = okMap
          .groupBy(_._1._1)
          .map { case (i, values) =>
            val possibilities = values.filter(tuple => tuple._2 && !resultMap.contains(tuple._1._2))
            (i, possibilities, possibilities.size)
          }
          .filter(_._3 == 1)
          .toList

        assert(result.size == 1)
        val (r, map, _) = result.head

        helper(resultMap.updated(map.head._1._2, map.head._1._1))
      }
    }

    //Map ruleId --> position
    val ruleLayout: Map[Int, Int] = helper(Map.empty)

    //ruleName -> index of rule
    val relevantRules = rules.zipWithIndex.filter(_._1._1.startsWith("departure")).map(t => (t._1._1, t._2))

    val myTicket = yourTicket.last.split(",").map(_.toInt).toList
    val relevantTicketValues = relevantRules.map { case (ruleId, ruleIndex) =>
      val rulePosition = ruleLayout(ruleIndex)
      myTicket(rulePosition).toLong
    }

    val productOfTicketValues = relevantTicketValues.product

    productOfTicketValues

  }
}

object Day16 {

  case class ParseResult(
      rules: List[(String, (List[Range.Inclusive], Set[Int]))],
      yourTicket: List[String],
      nearbyTickets: List[String],
      ticketValues: List[List[Int]]
  )

  def parse(lines: List[String]): ParseResult = {
    val rules: List[(String, (List[Range.Inclusive], Set[Int]))] = lines
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

    val yourTicket: List[String] = lines.dropWhile(line => !line.startsWith("your")).takeWhile(_.nonEmpty)

    val nearbyTickets: List[String] = lines.dropWhile(line => !line.startsWith("nearby")).drop(1)
    val ticketValues: List[List[Int]] = nearbyTickets.map(line => line.split(",").map(_.toInt).toList)

    ParseResult(rules, yourTicket, nearbyTickets, ticketValues)
  }

}
