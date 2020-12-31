package day20

import day20.Day20._
import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.nio.file.Files
import javax.imageio.ImageIO

class Day20Part1Test extends AnyFunSuite with Matchers {
  private val input = Helper.source(Some("src/main/resources/day20-example.txt")).mkString
  private val tileMap = parseTiles(input)
  private val transformedTileMap = getTileTransformations(tileMap)

  test("example 1") {

    val cornerPieces = findCornerPieces(transformedTileMap)

    cornerPieces should contain theSameElementsAs List(1951, 3079, 2971, 1171)

    val solution = cornerPieces.map(_.toLong).product

    println(s"solution part1: $solution")

    //writeTransformedTilesToDisk(transformedTileMap)
  }

  test("find corners of outer edges") {
    val neighbors = findNeighbors(transformedTileMap)
    val graph = createGraph(neighbors)
    val edgesUnsorted = outerEdgesUnsorted(neighbors, graph)

    val paths = edgesUnsorted
      .sortBy(_._3 * -1)

    paths.map { case (from, to, _, _) => (from, to) } should contain theSameElementsAs List(
      (1951, 3079),
      (1951, 2971),
      (1171, 2971),
      (1171, 3079)
    )

    paths.foreach(println)
  }

  test("orient edges") {
    val neighbors = findNeighbors(transformedTileMap)
    val graph = createGraph(neighbors)
    val edgesUnsorted = outerEdgesUnsorted(neighbors, graph)

    val edges = orientEdges(edgesUnsorted)

    edges shouldBe Edges(
      topEdgeLeftToRight = List(1171, 1489, 2971),
      leftEdgeTopToBottom = List(1171, 2473, 3079),
      rightEdgeTopToBottom = List(2971, 2729, 1951),
      bottomEdgeLeftToRight = List(3079, 2311, 1951)
    )

  }

  test("assemble outer edges") {
    val neighbors = findNeighbors(transformedTileMap)
    val graph = createGraph(neighbors)
    val edgesUnsorted = outerEdgesUnsorted(neighbors, graph)

    val edges = orientEdges(edgesUnsorted)
    val outerEdges = assembleVectorWithOuterEdges(edges)

    val expected = Vector(
      Vector(Some(1171), Some(1489), Some(2971)),
      Vector(Some(2473), None, Some(2729)),
      Vector(Some(3079), Some(2311), Some(1951))
    )

    outerEdges shouldBe expected
  }

  test("assemble complete picture") {
    val allTiles = assemblePicture(transformedTileMap)

    val expected = Vector(
      Vector(1171, 1489, 2971),
      Vector(2473, 1427, 2729),
      Vector(3079, 2311, 1951)
    )

    allTiles shouldBe expected
  }

  def writeTransformedTilesToDisk(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): Unit = {
    val tempFolder = Files.createTempDirectory("AOC2020_day20_")
    transformedTileMap.foreach { case (id, (original, transformations)) =>
      val filenameOriginal = s"$id.png"
      ImageIO.write(original, "png", tempFolder.resolve(filenameOriginal).toFile)
      transformations.foreach { case (transformationList, image) =>
        val filenameTransformed = s"$id - ${transformationList.mkString}.png"
        ImageIO.write(image, "png", tempFolder.resolve(filenameTransformed).toFile)
      }
    }
    println(s"wrote files to $tempFolder")
  }

  test("which transformations yield the same result") {
    val startImage = new BufferedImage(3, 3, BufferedImage.TYPE_INT_RGB)
    val coords: List[(Int, Int, Int)] = for {
      x <- 0.until(startImage.getWidth).toList
      y <- 0.until(startImage.getHeight).toList
    } yield (x, y, (x * 3 + y) * 28)

    coords.foreach { case (x, y, colorValue) => startImage.setRGB(x, y, new Color(colorValue, 0, 0).getRGB) }

    val transformedImages = allTransformations
      .map(transformationList => (transformationList, applyTransformations(startImage, transformationList)))
      .map { case (transformationList, img) => (transformationList, img, getImageHash(img, coords)) }

    val grouped = transformedImages.groupBy(_._3)
    grouped.foreach(println)

    val distinctTransformations = grouped.map(_._2.head._1)

    println(s"all ${allTransformations.size} transformations")

    allTransformations
      .map(_.mkString(","))
      .sorted
      .foreach(println)

    println(s"all ${distinctTransformations.size} distinct transformations")

    distinctTransformations.toList
      .map(_.mkString(","))
      .sorted
      .foreach(println)

  }

  def getImageHash(bufferedImage: BufferedImage, coords: List[(Int, Int, Int)]) = {
    coords.foldLeft("") { case (acc, (x, y, _)) =>
      val color = new Color(bufferedImage.getRGB(x, y))
      val r = color.getRed
      acc + r.toString
    }
  }

}

/*
found matching edge for t1: 1171 and t2: 1489. Id2 transformation = Clockwise180
found matching edge for t1: 1171 and t2: 2473. Id2 transformation = Clockwise270
found matching edge for t1: 1427 and t2: 1489. Id2 transformation = NoOp
found matching edge for t1: 1427 and t2: 2311. Id2 transformation = NoOp
found matching edge for t1: 1427 and t2: 2473. Id2 transformation = Clockwise90
found matching edge for t1: 1427 and t2: 2729. Id2 transformation = NoOp
found matching edge for t1: 1489 and t2: 2971. Id2 transformation = NoOp
found matching edge for t1: 1951 and t2: 2311. Id2 transformation = NoOp
found matching edge for t1: 1951 and t2: 2729. Id2 transformation = NoOp
found matching edge for t1: 2311 and t2: 3079. Id2 transformation = FlipVerticalClockwise180
found matching edge for t1: 2311 and t2: 3079. Id2 transformation = Clockwise180FlipVertical
found matching edge for t1: 2311 and t2: 3079. Id2 transformation = FlipHorizontal
found matching edge for t1: 2473 and t2: 3079. Id2 transformation = Clockwise270FlipVertical
found matching edge for t1: 2473 and t2: 3079. Id2 transformation = Clockwise90FlipHorizontal
found matching edge for t1: 2729 and t2: 2971. Id2 transformation = NoOp
 */
