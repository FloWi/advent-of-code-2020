package day15

// start: 18:48
//   end: 19:54
//      =  1:06h

import helper.Helper._

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve: List[String] => Long = Day15.solve(_, 2020)
}
object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }
  def solve: List[String] => Long = Day15.solve(_, 30000000)

}

object Day15 {

  def solve(lines: List[String], relevantNumber: Int): Long = {

    val numbers = lines.head.split(",").map(_.toInt).toVector

    def helper(currentRound: Int, lastNumberSpoken: Int, spokenAtRound: Map[Int, List[Int]]): Int = {

      if (currentRound <= 10) {
        println(s"""  current round: $currentRound
             |  lastNumberSpoken: $lastNumberSpoken
             |  spokenAtRound:
             |${spokenAtRound.map(kv => s"    $kv").mkString("\n")}
             |""".stripMargin.trim)
      }

      if (currentRound > relevantNumber) {
        lastNumberSpoken
      } else {
        val response = if (currentRound - 1 < numbers.length) {
          numbers(currentRound - 1)
        } else {
          spokenAtRound.get(lastNumberSpoken) match {
            case None                                  => 0
            case Some(_ :: Nil)                        => 0
            case Some(lastRound :: previousRound :: _) => lastRound - previousRound
          }
        }
        if (currentRound % 1000000 == 0) {
          println(s"  round $currentRound; response: $response; numbers spoken: ${spokenAtRound.size}\n")
        }
        helper(currentRound + 1, response, appendSpokenMap(response, currentRound, spokenAtRound))
      }

    }
    def appendSpokenMap(number: Int, round: Int, map: Map[Int, List[Int]]): Map[Int, List[Int]] = {
      val list = map.getOrElse(number, List.empty)
      map.updated(number, (round :: list).distinct.take(2))
    }

    helper(1, -1, Map.empty)
  }
}
