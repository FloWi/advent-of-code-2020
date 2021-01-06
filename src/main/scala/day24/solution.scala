package day24

import com.typesafe.scalalogging.LazyLogging
import day24.Day24.Cube._
import day24.Day24.TileColor.flip
import day24.Day24.{Cube, TileColor, parseInitialState}
import day24.gridHex.Hex
import helper.Helper._

import scala.annotation.tailrec

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val blackTiles = parseInitialState(lines).filter(_._2 == TileColor.Black)
    blackTiles.size
  }
}
object part2 extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines.toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    val initial = parseInitialState(lines)
    val actualStates = 1
      .to(100)
      .scanLeft((0, initial)) { case ((_, map), day) =>
        (day, part2.evaluateMove(map))
      }

    val finalState = actualStates.last._2

    val blackTiles = finalState.filter(_._2 == TileColor.Black)
    blackTiles.size
  }

  def evaluateMove(current: Map[Cube, TileColor]): Map[Cube, TileColor] = {

    // Rules:
    // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
    // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

    val blackTiles = current.filter(_._2 == TileColor.Black)

    val blackNeighbors = blackTiles.keySet.flatMap(cube => Cube.neighbors.values.map(offset => offset + cube))
    val additionalTiles = blackNeighbors.diff(current.keySet)
    val updatedMapWithNewTilesAsWhite = current ++ additionalTiles.map(c => (c, TileColor.White)).toMap

    val tilesToFlip = updatedMapWithNewTilesAsWhite.toList
      .filter { case (cube, color) =>
        val neighborTiles = Cube.neighbors.values.map(_ + cube)

        val neighbors = neighborTiles
          .filter(updatedMapWithNewTilesAsWhite.contains)
          .map(neighbor => (neighbor, updatedMapWithNewTilesAsWhite(neighbor)))
          .toList

        val blackNeighbors = neighbors
          .filter(_._2 == TileColor.Black)

        val numberOfBlackNeighborTiles = blackNeighbors.size

        val isBeingFlipped = color match {
          case TileColor.White => numberOfBlackNeighborTiles == 2
          case TileColor.Black => numberOfBlackNeighborTiles == 0 || numberOfBlackNeighborTiles > 2
        }
        //logger.debug(s"${cube.toHex.show} is $color and has $numberOfBlackNeighborTiles black neighbor tiles. Will be flipped: $isBeingFlipped")

        isBeingFlipped
      }

    val result = tilesToFlip.foldLeft(updatedMapWithNewTilesAsWhite) { case (map, (cube, currentColor)) =>
      map.updated(cube, flip(currentColor))
    }
    result

  }
}

object Day24 {

  def parseInitialState(lines: List[String]): Map[Cube, TileColor] = {
    val directionsToTiles = lines.map(parseDirections)
    val tilesToBeFlipped = directionsToTiles.map(findCubeAt(start = Cube(0, 0, 0), _))

    val flipCount = tilesToBeFlipped.groupBy(identity).map { case (cube, list) => (cube, list.size) }

    val whiteTiles = flipCount.filter(_._2 % 2 == 0).keys
    val blackTiles = flipCount.filter(_._2 % 2 != 0).keys

    val tilesMap: Map[Cube, TileColor] =
      (whiteTiles.map(t => (t, TileColor.White)) ++ blackTiles.map(t => (t, TileColor.Black))).toMap

    val neighborsOfBlackTiles = blackTiles.flatMap(t => neighbors.values.map(_ + t)).toSet

    val diff = neighborsOfBlackTiles.diff(tilesMap.keySet)
    val additionalWhites = diff.map(c => c -> TileColor.White)

    val finalMap = tilesMap ++ additionalWhites
    finalMap
  }

  sealed trait TileColor

  object TileColor {

    def flip(current: TileColor): TileColor = {
      current match {
        case White => Black
        case Black => White
      }
    }

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
      Direction.East -> Cube(1, -1, 0),
      Direction.Northeast -> Cube(1, 0, -1),
      Direction.Northwest -> Cube(0, 1, -1),
      Direction.West -> Cube(-1, 1, 0),
      Direction.Southwest -> Cube(-1, 0, 1),
      Direction.Southeast -> Cube(0, -1, 1)
    )

    implicit class ExtendedCube(val current: Cube) extends AnyVal {
      def +(other: Cube): Cube = {
        val Cube(x1, y1, z1) = current
        val Cube(x2, y2, z2) = other

        Cube(x1 + x2, y1 + y2, z1 + z2)
      }

      def toHex: Hex = {

        Hex.apply(q = current.x, r = current.z)
        //new Hex(current.x, current.y, current.z)
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
