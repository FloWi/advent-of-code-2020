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

    writeTransformedTilesToDisk(transformedTileMap)
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

  test("transform tiles to match") {

    val edges = calcEdges(transformedTileMap)

    val edgeMatches: Map[Int, List[Int]] = edges.toList
      .flatMap { case (id, (_, edgeMap)) =>
        val edgeHashes = edgeMap.values.flatMap(_._2)
        edgeHashes.map(hash => (hash, id))
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2).distinct)
      .toMap

    val leftTop = edges(1951)
    val middleTop = edges(2311)
    val leftMiddle = edges(2729)

    def printEdges(img: BufferedImage, additional: Option[String] = None): Unit = {

      val t = top(img)
      val b = bottom(img)
      val l = left(img)
      val r = right(img)

      println(s"top: $t; right: $r; bottom: $b; left: $l${additional.getOrElse("")}")
    }

    printEdges(leftTop._1, Some("; top-left original"))

    val leftTopTransformations = leftTop._2.toList.map { case (transformations, (img, edgeHashes)) => (transformations, img, edgeHashes.toSet) }
    val middleTopTransformations = middleTop._2.toList.map { case (transformations, (img, edgeHashes)) => (transformations, img, edgeHashes.toSet) }
    val leftMiddleTransformations = leftMiddle._2.toList.map { case (transformations, (img, edgeHashes)) => (transformations, img, edgeHashes.toSet) }

    //find two edges, that only the top-left tile has (the top one and the left one)
    val outerEdgesOfTopLeftTile = edgeMatches.filter(_._2 == List(1951)).keys.toSet
    println(s"edges of top left tile without matching other tile - these have to be on the outer side: ${outerEdgesOfTopLeftTile.toList.sorted.mkString(", ")}")

    //find the transformations, after which those edges are top and left
    val validTransformationsLeftTop = leftTopTransformations.filter { case (_, img, _) =>
      val isTop = outerEdgesOfTopLeftTile.contains(top(img))
      val isLeft = outerEdgesOfTopLeftTile.contains(left(img))
      isTop && isLeft
    }
    validTransformationsLeftTop.foreach(tup => printEdges(tup._2, Some(s"; valid transformations: ${tup._1}")))

    //find right edges of topLeft tile after transformations
    val rightEdgesOfTopLeftAfterTransformations = validTransformationsLeftTop.map(_._2).map(right).toSet
    // 318, 710

    //find bottom edges of topLeft tile after transformations
    val bottomEdgesOfTopLeftAfterTransformations = validTransformationsLeftTop.map(_._2).map(bottom).toSet
    // also 318, 710

    println()
    printEdges(middleTop._1, Some("; middleTop original"))

    val validTransformationsOfMiddleTop = middleTopTransformations.filter(t => rightEdgesOfTopLeftAfterTransformations.contains(left(t._2)))
    println(s"found ${validTransformationsOfMiddleTop.size} transformations, where the left side of middleTop matches the right side of leftTop")
    validTransformationsOfMiddleTop.foreach { case (transformations, img, _) =>
      printEdges(img, Some(s"; transformations: $transformations"))
    }
    println()
    printEdges(leftMiddle._1, Some("; leftMiddle original"))

    val validTransformationsOfLeftMiddle = leftMiddleTransformations.filter(t => bottomEdgesOfTopLeftAfterTransformations.contains(top(t._2)))
    println(s"found ${validTransformationsOfLeftMiddle.size} transformations, where the top side of leftMiddle matches the bottom side of leftTop")
    validTransformationsOfLeftMiddle.foreach { case (transformations, img, _) =>
      printEdges(img, Some(s"; transformations: $transformations"))
    }

    ???

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
    val startImage = new BufferedImage(4, 4, BufferedImage.TYPE_INT_RGB)
    val coords: List[(Int, Int, Int)] = for {
      x <- 0.until(startImage.getWidth).toList
      y <- 0.until(startImage.getHeight).toList
    } yield (x, y, (x * 4 + y) * 15)

    coords.foreach { case (x, y, colorValue) => startImage.setRGB(x, y, new Color(colorValue, 0, 0).getRGB) }
    val tempFolder = Files.createTempDirectory("AOC2020_day20_")
    ImageIO.write(startImage, "png", tempFolder.resolve(s"start_image - hash - ${getImageHash(startImage, coords)}.png").toFile)

    val transformedImages = allTransformations
      .map(transformationList => (transformationList, applyTransformations(startImage, transformationList)))
      .map { case (transformationList, img) => (transformationList, img, getImageHash(img, coords)) }

    transformedImages.foreach { case (transformations, img, hash) =>
      ImageIO.write(img, "png", tempFolder.resolve(s"image - hash - $hash - ${transformations.mkString}.png").toFile)

    }
    val grouped = transformedImages.groupBy(_._3)
    grouped.foreach(println)

    val distinctTransformations = grouped.map(_._2.sortBy(_.toString.size).head._1)

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
