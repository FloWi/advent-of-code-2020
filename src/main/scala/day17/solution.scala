package day17

import helper.Helper._

import scala.annotation.tailrec

object part1 {

  import Day17._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    val activeCubes = parseActiveCubes((x, y) => (x, y, 0))(lines)
    println(s"Initial Number of active cubes: ${activeCubes.size}")

    val activeAfterAdvancing = 1.to(6).foldLeft(activeCubes) { (current, _) =>
      advanceStep(getNeighbors3D)(current)
    }

    println(s"Number of active cubes after 6 rounds: ${activeAfterAdvancing.size}")

    activeAfterAdvancing.size
  }
}
object part2 {

  import Day17._

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    val activeCubes = parseActiveCubes((x, y) => (x, y, 0, 0))(lines)
    println(s"Initial Number of active cubes: ${activeCubes.size}")

    val activeAfterAdvancing = 1.to(6).foldLeft(activeCubes) { (current, _) =>
      advanceStep(getNeighbors4D)(current)
    }

    println(s"Number of active cubes after 6 rounds: ${activeAfterAdvancing.size}")

    activeAfterAdvancing.size
  }
}

object Day17 {
  def parseActiveCubes[Tup](createCoord: (Int, Int) => Tup)(lines: List[String]): Set[Tup] = {
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.flatMap { case (char, x) =>
        if (char == '#') List(createCoord(x, y)) else List.empty
      }
    }.toSet
  }

  def advanceStep[Tup](getNeighbors: Tup => Set[Tup])(activeCubes: Set[Tup]): Set[Tup] = {
    //If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
    //If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

    val rule1ActiveCubes = activeCubes.filter { cube =>
      val activeNeighbors = getNeighbors(cube).count(n => activeCubes.contains(n))
      activeNeighbors == 2 || activeNeighbors == 3
    }

    val allRelevantCubes = activeCubes.flatMap(getNeighbors) ++ activeCubes
    val allInActive = allRelevantCubes.diff(activeCubes)
    val rule2ActiveCubes = allInActive.filter { cube =>
      val activeNeighbors = getNeighbors(cube).count(n => activeCubes.contains(n))
      activeNeighbors == 3
    }

    rule1ActiveCubes ++ rule2ActiveCubes
  }

  def getNeighbors3D(cube: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    val (x, y, z) = cube
    for {
      dx <- (-1).to(1)
      dy <- (-1).to(1)
      dz <- (-1).to(1)
      neighbor = (x + dx, y + dy, z + dz)
      if neighbor != cube
    } yield neighbor
  }.toSet

  def getNeighbors4D(cube: (Int, Int, Int, Int)): Set[(Int, Int, Int, Int)] = {
    val (x, y, z, w) = cube
    for {
      dx <- (-1).to(1)
      dy <- (-1).to(1)
      dz <- (-1).to(1)
      dw <- (-1).to(1)
      neighbor = (x + dx, y + dy, z + dz, w + dw)
      if neighbor != cube
    } yield neighbor
  }.toSet
}
