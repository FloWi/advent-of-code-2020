package day13

import day13.Day13.Bus
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day13Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    day13.part2.solve(Bus.parse("7,13,x,x,59,x,31,19")) should ===(1068781L)

  }

  test("example 2") {
    day13.part2.solve(Bus.parse("67,7,59,61")) should ===(754018L)

  }

  test("example 3") {
    day13.part2.solve(Bus.parse("67,x,7,59,61")) should ===(779210L)

  }

  test("example 4") {
    day13.part2.solve(Bus.parse("67,7,x,59,61")) should ===(1261476L)

  }

  test("example 5") {
    day13.part2.solve(Bus.parse("1789,37,47,1889")) should ===(1202161486L)

  }

}
