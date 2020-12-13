package day13

// part 1
// start: 16:05
//   end: 16:17
//      =  0:12h

//part 2
// start: 16:18
//   end: 17:45
//      =  1:27h

import helper.Helper._

object part1 {

  import Day13._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector

    val earliestDepartureTimestamp = input.head.toInt
    val busses = Bus.parse(input.last)

    println(s"earliest ts: $earliestDepartureTimestamp")
    println(s"busses with ids:")
    busses.foreach(println)

    val solution = solve(earliestDepartureTimestamp, busses)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(ts: Int, busses: Vector[Bus]): Int = {
    val (_, waitTime, bestBus) = busses
      .map { case bus @ Bus(id, _) =>
        val busDepartTime = ((ts / id) + 1) * id
        val waitTime = busDepartTime - ts
        println(s"busDepartTime: $busDepartTime, waitTime: $waitTime, bus: $bus")
        (busDepartTime, waitTime, bus)
      }
      .minBy(_._2)

    waitTime * bestBus.id
  }
}

object part2 {

  import Day13._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector

    val busses = Bus.parse(input.last)
    val solution = solve(busses)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(busses: Vector[Bus]): Long = {

    val (currentT, _) = busses.foldLeft((0L, 1L)) { case ((currentT, productOfIds), Bus(id, index)) =>
      val newT = LazyList
        .from(1)
        .map(i => currentT + productOfIds * i)
        .find(t => (t + index) % id == 0)
        .get // let it crash - of course we find something :)

      val newP = productOfIds * id
      println(s"\nsolving for constraint (t + $index) % $id == 0")
      println(s"currentT: $newT; product of bus ids so far: $newP")
      (newT, newP)
    }

    currentT
  }
}

object Day13 {

  object Bus {
    def parse(line: String): Vector[Bus] = {
      line
        .split(",")
        .zipWithIndex
        .filterNot(_._1 == "x")
        .map { case (busIdString, index) => Bus(busIdString.toInt, index) }
        .toVector
    }
  }

  case class Bus(id: Int, index: Int)
}

/*
Explanation for part 2.

kudos to @lizthegrey for pointing out this elegant way of solving it
example:
constraints for finding t
(t + 0) % 7 == 0
(t + 1) % 13 == 0
(t + 4) % 59 == 0
(t + 6) % 31 == 0
(t + 7) % 19 == 0

solve iteratively
first solution is t=7 --> 7 + 0 = 7 ==> 7 % 7 = 0
now try to find solution for this constraint: (t + 1) % 13 == 0
add 7 until you find a number that satisfies this constraint.
This is possible, because multiplying adding 7 doesn't violate the "% 7 constraint"
Solution is t=77 --> 77 + 1 = 78 ==> 78 % 13 = 0
Take the product of both divisors ==> 7 * 13 = 91

If you add 91 to the current t = 77, you don't violate both 1st and 2nd constraint
Proof: t = 77 + 91 = 168 ==>
(168 + 0) % 7 == 0
(168 + 1) % 13 == 0

repeat 3x until you find
t = 77+91*3 = 350

repeat until you processed all entries


here is the state throughout the solution

solving for constraint (t + 0) % 7 == 0
currentT: 7; product of bus ids so far: 7

solving for constraint (t + 1) % 13 == 0
currentT: 77; product of bus ids so far: 91

solving for constraint (t + 4) % 59 == 0
currentT: 350; product of bus ids so far: 5369

solving for constraint (t + 6) % 31 == 0
currentT: 70147; product of bus ids so far: 166439

solving for constraint (t + 7) % 19 == 0
currentT: 1068781; product of bus ids so far: 3162341

 */
