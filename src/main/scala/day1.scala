import scala.io.Source
object day1Part1 {
  def main(args: Array[String]) = {

    /*
    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     */
    val input =
      Source
        .fromResource("day1.txt")
        .getLines()
        .filterNot(_.isEmpty())
        .toList

    println(input)

    val res = input
      .map(_.toInt)
      .toList
      .combinations(2)
      .toList
      .map { case List(a, b) => (a, b, a + b) }
      .find { case (_, _, sum) => sum == 2020 }
      .get

    val (a, b, _) = res
    println(s"a: $a, b: $b, result: ${a * b}")
  }

}

object day1Part2 {
  def main(args: Array[String]) = {

    /*
    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     */
    val input =
      Source
        .fromResource("day1.txt")
        .getLines()
        .filterNot(_.isEmpty())
        .toList

    println(input)

    val res = input
      .map(_.toInt)
      .toList
      .combinations(3)
      .toList
      .map { case List(a, b, c) => (a, b, c, a + b + c) }
      .find { case (_, _, _, sum) => sum == 2020 }
      .get

    val (a, b, c, _) = res
    println(s"a: $a, b: $b, c: $c, result: ${a * b * c}")
  }

}
