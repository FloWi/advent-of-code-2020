package day7

// Start: 8:52
// Break: 9:29
// 28min
//
// Start: 19:46
// multiple print statements later ...
// End:   21:18
// 92 min
//
// Total: 120min

import helper.Helper._

object day7 {
  case class Color(name: String) extends AnyVal
  case class Quantity(qty: Int) extends AnyVal

  object LuggageInstruction {

    def parse(line: String) = {

      line.split("bags contain ").toList match {
        case List(outerBag, otherBags) =>
          Color(outerBag.trim) -> parseInnerBags(otherBags)
        case _ => throw new RuntimeException(s"can't parse. line: '$line'")
      }
    }

    def parseInnerBags(str: String): Map[Color, Quantity] = {
      //1 bright white bag, 2 muted yellow bags.
      //no other bags.

      if (str.contains("no other bags")) {
        Map.empty
      } else {
        str.replace(".", "").split(", ").toList.map { s =>
          //println(s"parsing inner bag: '$s'")
          val bagChunks = s.split(" ").map(_.trim).filterNot(_.isEmpty)
          val qty = bagChunks.head.toInt
          val color = bagChunks.slice(1, 3).mkString(" ")
          Color(color) -> Quantity(qty)
        }
      }.toMap
    }
  }

  def search(map: LuggageInstructions, destinationColor: Color): List[Color] = {

    def findColor(
        current: Color,
        depth: Int = 0
    ): Boolean = {
      if (current == destinationColor) true
      else {
        map.get(current) match {
          case Some(subMap) =>
            val result = subMap.exists(tup => findColor(tup._1, depth + 1))
            result
          case None =>
            false
        }
      }
    }

    val result = map.keys
      .filterNot(c => c == destinationColor)
      .filter(c => findColor(c))
      .toList

    println(result)
    result
  }

  def searchPart2(map: LuggageInstructions, startingColor: Color): Int = {
    //starting from one color traversing the whole tree down

    def helper(currentColor: Color, currentResult: Int = 1): Int = {
      map.get(currentColor) match {
        case Some(subMap) if subMap.isEmpty =>
          currentResult

        case Some(subMap) =>
          val result = subMap.map { case (color, qty) =>
            helper(color, qty.qty * currentResult)
          }.sum
          result + (if (currentColor == startingColor) 0 else currentResult)

        case None =>
          currentResult
      }
    }

    helper(startingColor)
  }

  type LuggageInstructions = Map[Color, Map[Color, Quantity]]
}

object part1 {

  import day7._

  def main(args: Array[String]) = {

    val input = source(args.headOption).getLines.toList.filterNot(_.isEmpty)

    val luggageInstructions: LuggageInstructions =
      input.map(LuggageInstruction.parse).toMap

    println("luggage instructions")
    luggageInstructions.foreach { case (color, subMap) =>
      println(s"color: '${color.name}'")
      subMap.foreach { case (subColor, qty) =>
        println(s"  color: '${subColor.name}'; qty: ${qty.qty}")
      }
    }

    val result = search(luggageInstructions, Color("shiny gold"))

    val solution = result.distinct.size

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }
}

object part2 {

  import day7._

  def main(args: Array[String]) = {

    val input = source(args.headOption).getLines.toList.filterNot(_.isEmpty)

    val luggageInstructions: LuggageInstructions =
      input.map(LuggageInstruction.parse).toMap

    val result = searchPart2(luggageInstructions, Color("shiny gold"))

    println(
      s"Solution for ${getCallingMainClass} is: $result"
    )
  }
}
