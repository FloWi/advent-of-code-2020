package day20

import day20.Day20._
import helper.Helper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.nio.file.Files
import javax.imageio.ImageIO

class Day20Test extends AnyFunSuite with Matchers {
  private val input = Helper.source(Some("src/main/resources/day20-example.txt")).mkString
  private val inputSeaMonster = Helper.source(Some("src/main/resources/day20-sea-monster.txt")).mkString
  private val inputExpectedAssemblyWithoutBorders = Helper.source(Some("src/main/resources/day20-expected-assembly-without-borders.txt")).mkString
  private val inputExpectedFinal = Helper.source(Some("src/main/resources/day20-expected-final-image.txt")).mkString
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

  test("assemble tiles") {
    val allTiles = assembleTiles(transformedTileMap)
      .map(_.map(_._1))

    val expected = Vector(
      Vector(1951, 2311, 3079),
      Vector(2729, 1427, 2473),
      Vector(2971, 1489, 1171)
    )

    allTiles shouldBe expected
  }

  test("assemble complete pictures") {
    val allTiles = assembleTiles(transformedTileMap)

    val pictures = assemblePicture(allTiles)
    pictures.withBorders.getWidth shouldBe pictures.withoutBorders.getWidth + math.sqrt(transformedTileMap.size).toInt * 2

    val transformedImages = distinctTransformations.map { transformations =>
      val withBorders = applyTransformations(pictures.withBorders, transformations)
      val withRedBorders = applyTransformations(pictures.imgWithRedRedSeparatorBorders, transformations)
      val withoutBorders = applyTransformations(pictures.withoutBorders, transformations)
      (transformations, FinalImages(withBorders, withRedBorders, withoutBorders))
    }

    val tempFolder = Files.createTempDirectory("AOC2020_day20_")
    transformedImages.foreach { case (transformations, FinalImages(withBorders, withRedBorders, withoutBorders)) =>
      ImageIO.write(withBorders, "png", tempFolder.resolve(s"final_with_borders_${transformations.mkString}.png").toFile)
      ImageIO.write(withRedBorders, "png", tempFolder.resolve(s"final_with_red_borders_${transformations.mkString}.png").toFile)
      ImageIO.write(withoutBorders, "png", tempFolder.resolve(s"final_without_borders_${transformations.mkString}.png").toFile)
    }
    println(s"wrote files to $tempFolder")

  }

  test("sea monster and final image") {
    val solution = part2.solve(input, inputSeaMonster)

    val tempFolder = Files.createTempDirectory("AOC2020_day20_")

    ImageIO.write(solution.image, "png", tempFolder.resolve(s"final_image_${solution.transformations.mkString}.png").toFile)
    val imgWithGreenSeaMonster = new BufferedImage(solution.image.getWidth, solution.image.getHeight, BufferedImage.TYPE_INT_RGB)

    val brown = new Color(102, 51, 0)

    0
      .until(solution.image.getWidth)
      .foreach { x =>
        0.until(solution.image.getHeight)
          .foreach { y =>
            val coord = (x, y)
            val color =
              if (solution.seaMonsterPixels.contains(coord)) Color.green.darker
              else {
                if (solution.image.getRGB(x, y) == Color.WHITE.getRGB) Color.BLUE
                else {
                  brown
                }
              }
            imgWithGreenSeaMonster.setRGB(x, y, color.getRGB)
          }
      }

    ImageIO.write(imgWithGreenSeaMonster, "png", tempFolder.resolve(s"final_image_colored.png").toFile)

  }

  test("test finding sea monsters in example image") {
    val seaMonster = parseImage(inputSeaMonster.split("\n").toList)
    val expectedAssemlbyWithoutBorders = parseImage(inputExpectedAssemblyWithoutBorders.split("\n").toList)
    val expectedfinalImage = parseImage(inputExpectedFinal.split("\n").toList)

    findSeaMonsters(expectedfinalImage, seaMonster).map(_.topLeftCoordinates) should contain theSameElementsAs List((1, 16), (2, 2))
    findSeaMonsters(expectedAssemlbyWithoutBorders, seaMonster).map(_.topLeftCoordinates) should contain theSameElementsAs List.empty
  }

  test("print edge map") {
    val edges = calcEdges(transformedTileMap)

    println("tile_id;top;right;left;bottom;transformations")
    edges.foreach { case (id, (_, edgeMap)) =>
      edgeMap.foreach { case (transformations, (_, edge)) =>
        println(s"$id;${edge.top};${edge.right};${edge.bottom};${edge.left};${transformations.mkString(", ")}")
      }
    }
  }

  def writeTransformedTilesToDisk(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): Unit = {
    val tempFolder = Files.createTempDirectory("AOC2020_day20_")
    transformedTileMap.foreach { case (id, (original, transformations)) =>
      val filenameOriginal = s"$id.png"
      ImageIO.write(original, "png", tempFolder.resolve(filenameOriginal).toFile)
      transformations.foreach { case (transformationList, image) =>
        val filenameTransformed = s"${id}_${transformationList.mkString}.png"
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
    ImageIO.write(startImage, "png", tempFolder.resolve(s"start_image_hash_${getImageHash(startImage, coords)}.png").toFile)

    val transformedImages = allTransformations
      .map(transformationList => (transformationList, applyTransformations(startImage, transformationList)))
      .map { case (transformationList, img) => (transformationList, img, getImageHash(img, coords)) }

    transformedImages.foreach { case (transformations, img, hash) =>
      ImageIO.write(img, "png", tempFolder.resolve(s"image_hash_${hash}_${transformations.mkString}.png").toFile)

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

  test("documentation output for edge matching") {
    val edges = calcEdges(transformedTileMap)

    val edgeList: List[(Int, Edge, List[Transformation])] = edges.flatMap { case (id, (_, edgeMap)) =>
      edgeMap.map { case (transformations, (_, edges)) =>
        (id, edges, transformations)
      }
    }.toList

    val horizontalMatching = edgeList
      .map { case (id1, edges1, transformations1) =>
        val matching = edgeList.filter { case (id2, edges2, _) =>
          id1 != id2 && edges1.right == edges2.left
        }
        (id1, edges1, transformations1, matching)
      }
      .filterNot(_._4.isEmpty)
      .flatMap { case (id1, edge1, trans1, matches) =>
        matches.map { case (id2, _, trans2) =>
          (id1, trans1, id2, trans2, "horizontal", edge1.right)
        }
      }

    val verticalMatching = edgeList
      .map { case (id1, edges1, transformations1) =>
        val matching = edgeList.filter { case (id2, edges2, _) =>
          id1 != id2 && edges1.bottom == edges2.top
        }
        (id1, edges1, transformations1, matching)
      }
      .filterNot(_._4.isEmpty)
      .flatMap { case (id1, edge1, trans1, matches) =>
        matches.map { case (id2, _, trans2) =>
          (id1, trans1, id2, trans2, "vertical", edge1.top)
        }
      }

    val allMatches = horizontalMatching ++ verticalMatching

    println("\n\nmatching edges")
    println("tile1 id;transformation tile1;tile2 id;transformation tile2;match direction;hash of matching edge")
    allMatches
      .sortBy(t => (t._1, t._3))
      .foreach { case (id1, trans1, id2, trans2, direction, edgeHash) =>
        println(s"$id1;${trans1.mkString(", ")};$id2;${trans2.mkString(", ")};$direction;$edgeHash")
      }

    val numberOfMatchesWithOtherTiles = allMatches
      .map(t => (t._1, t._3))
      .distinct
      .groupBy(_._1)
      .view
      .mapValues { list => (list.size, list.map(_._2)) }
      .toList
      .sortBy(_._1)

    val neighbors = findNeighbors(transformedTileMap)

    val neighborDiGraph = neighbors
      .flatMap { list =>
        List(list, list.reverse)
      }

    val tilesWithNeighbors = neighborDiGraph
      .groupBy(_.head)
      .map { case (id, group) => (id, group.size, group.map(_.last)) }
      .toList

    println("\n\nnumber of matching tiles")
    println("tile id;number of matches;matching tiles")
    tilesWithNeighbors
      .sortBy { case (id, size, _) => (size, id) }
      .foreach { case (id, num, matchingTiles) =>
        println(s"$id;$num;${matchingTiles.sorted.mkString(",")}")
      }

    println("\n\nedge hashes of tiles")
    println("id;transformation;top;left;bottom;right")

    edgeList
      .filter(t => Set(1951, 2311, 2729, 2971, 3079).contains(t._1))
      .sortBy { case (id, e, _) => (id, e.top, e.left) }
      .foreach { case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
      }

    println("\n\nall edge hashes of 1951")
    edges(1951)._2.flatMap(_._2._2.edges).toList.distinct.sorted.foreach(println)

    println("\n\nall edge hashes that only exist once")

    val edgeUsage = edgeList
      .flatMap { case (id, edge, _) =>
        edge.edges.map(hash => (hash, id))
      }
      .distinct
      .groupBy(_._1)
      .view
      .map { case (hash, matches) => (hash, matches.size, matches.map(_._2)) }
      .toList
    println("tile ids;number of tiles with that edge;edge hash")
    edgeUsage
      .sortBy { case (edgeHash, cnt, matchingTiles) =>
        (matchingTiles.headOption, cnt, edgeHash)
      }
      .foreach { case (edgeHash, cnt, matchingTiles) =>
        println(s"${matchingTiles.sorted.mkString(", ")};$cnt;$edgeHash")
      }

    println("\n\ntile with two neighbors")
    println("id;transformation;top;left;bottom;right")

    edgeList
      .filter { case (_, e, _) =>
        //t.top == 210 && t.left == 9 ||
        //t.top == 9   && t.left == 210
        e.top == 210 && e.left == 9 || e.top == 9 && e.left == 210
      }
      .sortBy { case (id, e, _) => (id, e.top, e.left) }
      .foreach { case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
      }

    println("\n\ntop right with left neighbor-constraint")
    println("id;transformation;top;left;bottom;right")

    val otherNeighbors = Set(1171, 2971, 3079)
    edgeList
      .filter { t =>
        otherNeighbors.contains(t._1) && Set(85, 616).contains(t._2.left)
      }
      .sortBy { case (id, e, _) => (id, e.top, e.left) }
      .foreach { case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
      }

    println("\n\nbottom left with top neighbor-constraint")
    println("id;transformation;top;left;bottom;right")

    edgeList
      .filter { t =>
        otherNeighbors.contains(t._1) && Set(85, 616).contains(t._2.top)
      }
      .sortBy { case (id, e, _) => (id, e.top, e.left) }
      .foreach { case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
      }

    val tile_2_0 = edgeList.find { case (id, _, transformations) => id == 3079 && transformations == List(NoOp) }.get
    val tile_1_1 = edgeList.find { case (id, _, transformations) => id == 1427 && transformations == List(FlipVertical) }.get
    val tile_0_2 = edgeList.find { case (id, _, transformations) => id == 2971 && transformations == List(FlipVertical) }.get

    println("\n\n(1,2) with constraints")
    println("id;transformation;top;left;bottom;right")

    val tile_1_2 =
      edgeList.find { t =>
        t._2.top == tile_1_1._2.bottom && t._2.left == tile_0_2._2.right
      }.get

    tile_1_2 match {
      case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
    }

    println("\n\n(2,1) with constraints")
    println("id;transformation;top;left;bottom;right")

    val tile_2_1 =
      edgeList.find { t =>
        t._2.left == tile_1_1._2.right && t._2.top == tile_2_0._2.bottom
      }.get

    tile_2_1 match {
      case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
    }

    println("\n\n(2,2) with constraints")
    println("id;transformation;top;left;bottom;right")

    val tile_2_2 =
      edgeList.find { t =>
        t._2.left == tile_1_2._2.right && t._2.top == tile_2_1._2.bottom
      }.get

    tile_2_2 match {
      case (id, edge, transformations) =>
        println(s"$id;${transformations.mkString(", ")};${edge.top};${edge.left};${edge.bottom};${edge.right}")
    }

    println()
  }

  def getImageHash(bufferedImage: BufferedImage, coords: List[(Int, Int, Int)]) = {
    coords.foldLeft("") { case (acc, (x, y, _)) =>
      val color = new Color(bufferedImage.getRGB(x, y))
      val r = color.getRed
      acc + r.toString
    }
  }

}
