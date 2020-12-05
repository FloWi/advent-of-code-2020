import scala.io.Source
import java.nio.file.Paths

object day3Part1 {

  def main(args: Array[String]) = {
    /*
    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     */

    val filename = args.head

    val trees: Array[Array[Boolean]] =
      Source
        .fromFile(Paths.get(filename).toAbsolutePath().toString())
        .getLines()
        .map(_.toArray.map(_ == '#'))
        .toArray

    case class Slope(right: Int, down: Int)

    val slope = Slope(right = 3, down = 1)

    val rowIndices = slope.down.until(trees.length, slope.down).toSet

    trees
      .map(_.map(b => if (b) "#" else ".").mkString(""))
      .foreach(println)

    val numberOfTreeCollisions = trees.zipWithIndex
      .filter { case (_, rowIndex) => rowIndices.contains(rowIndex) }
      .map { case (row, rowIndex) =>
        val colIndex = rowIndex / slope.down * slope.right % row.size
        val encounter = if (row(colIndex)) 1 else 0
        println(
          s"rowindex: $rowIndex; colIndex: $colIndex; row.size: ${row.size}; encounter: $encounter"
        )

        encounter
      }
      .sum

    println(s"numberOfTreeCollisions: $numberOfTreeCollisions")
  }
}

object day3Part2 {

  def main(args: Array[String]) = {
    /*
    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     */

    val filename = args.head

    val trees: Array[Array[Boolean]] =
      Source
        .fromFile(Paths.get(filename).toAbsolutePath().toString())
        .getLines()
        .map(_.toArray.map(_ == '#'))
        .toArray

    case class Slope(right: Int, down: Int)

    val slopes = List(
      Slope(right = 1, down = 1),
      Slope(right = 3, down = 1),
      Slope(right = 5, down = 1),
      Slope(right = 7, down = 1),
      Slope(right = 1, down = 2)
    )

    val treeCollisions =
      slopes
        .map { slope =>
          val rowIndices = slope.down.until(trees.length, slope.down).toSet

          trees
            .map(_.map(b => if (b) "#" else ".").mkString(""))
            .foreach(println)

          val numberOfTreeCollisions = trees.zipWithIndex
            .filter { case (_, rowIndex) => rowIndices.contains(rowIndex) }
            .map { case (row, rowIndex) =>
              val colIndex = rowIndex / slope.down * slope.right % row.size
              val encounter = if (row(colIndex)) 1 else 0
              println(
                s"rowindex: $rowIndex; colIndex: $colIndex; row.size: ${row.size}; encounter: $encounter"
              )

              encounter
            }
            .sum
          numberOfTreeCollisions.toLong
        }

    println(s"treeCollisions: $treeCollisions")
    println(s"result: ${treeCollisions.product}")
  }
}

/*
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#


..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
 */
