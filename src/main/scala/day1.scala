import scala.io.Source

package day1 {

  import Helper._

  object part1 {

    def main(args: Array[String]) = {

      val input = source(args.headOption)
        .getLines()
        .filterNot(_.isEmpty())
        .toList

      println(input)

      val res = input
        .map(_.toInt)
        .toList
        .combinations(2)
        .toList
        .collect { case List(a, b) => (a, b, a + b) }
        .find { case (_, _, sum) => sum == 2020 }
        .get

      val (a, b, _) = res
      println(
        s"Solution for ${getCallingMainClass.getCanonicalName}: a: $a, b: $b, result: ${a * b}"
      )
    }
  }

  object part2 {

    def main(args: Array[String]) = {

      val input = source(args.headOption)
        .getLines()
        .filterNot(_.isEmpty())
        .toList

      println(input)

      val res = input
        .map(_.toInt)
        .toList
        .combinations(3)
        .toList
        .collect { case List(a, b, c) => (a, b, c, a + b + c) }
        .find { case (_, _, _, sum) => sum == 2020 }
        .get

      val (a, b, c, _) = res
      println(
        s"Solution for ${getCallingMainClass.getCanonicalName}: a: $a, b: $b, c: $c, result: ${a * b * c}"
      )
    }

  }

}
