package day24

import day24.Day24.Cube.findCubeAt
import day24.Day24.{Cube, TileColor, parseDirections}
import helper.Helper._

import scala.annotation.tailrec

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val directionsToTiles = lines.map(parseDirections)
    val tilesToBeFlipped = directionsToTiles.map(findCubeAt(start = Cube(0, 0, 0), _))
    val tilesSet = tilesToBeFlipped.toSet

    val tilesMap: Map[Cube, TileColor] = tilesSet.map(t => t -> TileColor.White).toMap

    val resultingMap = tilesToBeFlipped.foldLeft(tilesMap) { case (current, tile) =>
      val currentColor = current(tile)
      val newColor = if (currentColor == TileColor.White) TileColor.Black else TileColor.White

      current.updated(tile, newColor)
    }

    val blackTiles = resultingMap.filter(_._2 == TileColor.Black)
    blackTiles.size

  }
}

object Day24 {

  sealed trait TileColor

  object TileColor {

    case object White extends TileColor

    case object Black extends TileColor

  }
  def parseDirections(line: String): Seq[Direction] = {

    @tailrec
    def helper(rest: String, acc: List[Direction]): List[Direction] = {
      Direction.parse(rest) match {
        case None => acc.reverse
        case Some((matched, direction)) =>
          helper(rest.drop(matched.length), direction :: acc)

      }
    }

    helper(line, List.empty)
  }

  //https://www.redblobgames.com/grids/hexagons/#neighbors
  case class Cube(x: Int, y: Int, z: Int)

  object Cube {
    //clockwise starting east
    val neighbors: Map[Direction, Cube] = Map(
      Direction.East -> Cube(+1, -1, 0),
      Direction.Northeast -> Cube(+1, 0, -1),
      Direction.Northwest -> Cube(0, +1, -1),
      Direction.West -> Cube(-1, +1, 0),
      Direction.Southwest -> Cube(-1, 0, +1),
      Direction.Southeast -> Cube(0, -1, +1)
    )

    implicit class ExtendedCube(val current: Cube) extends AnyVal {
      def +(other: Cube): Cube = {
        val Cube(x1, y1, z1) = current
        val Cube(x2, y2, z2) = other

        Cube(x1 + x2, y1 + y2, z1 + z2)
      }
    }

    def findCubeAt(start: Cube, directions: Seq[Direction]): Cube = {
      directions.foldLeft(start) { case (cube, direction) =>
        val offset = neighbors(direction)
        cube + offset
      }
    }

  }
  sealed trait Direction {
    val name: String
    val abb: String
  }

  object Direction {

    def parse(line: String): Option[(String, Direction)] = {
      if (line.isEmpty) None
      else directionMap.keys.find(line.startsWith).map { beginning => (beginning, directionMap(beginning)) }
    }

    //Because the tiles are hexagonal, every tile has six neighbors:
    //east, southeast, southwest, west, northwest, and northeast.
    //These directions are given in your list, respectively, as
    //e, se, sw, w, nw, and ne.

    val directionMap: Map[String, Direction] = Map(
      "e" -> Direction.East,
      "se" -> Direction.Southeast,
      "sw" -> Direction.Southwest,
      "w" -> Direction.West,
      "nw" -> Direction.Northwest,
      "ne" -> Direction.Northeast
    )

    case object East extends Direction {
      val name = "East"
      val abb = "e"
    }

    case object Southeast extends Direction {
      val name = "Southeast"
      val abb = "se"
    }

    case object Southwest extends Direction {
      val name = "Southwest"
      val abb = "sw"
    }

    case object West extends Direction {
      val name = "West"
      val abb = "w"
    }

    case object Northwest extends Direction {
      val name = "Northwest"
      val abb = "nw"
    }

    case object Northeast extends Direction {
      val name = "Northeast"
      val abb = "ne"
    }

  }

}
