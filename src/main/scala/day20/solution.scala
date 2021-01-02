package day20

import day20.Day20._
import day20.Dijkstra.{Graph, shortestPath}
import helper.Helper
import helper.Helper._

import java.awt.Color
import java.awt.image.BufferedImage

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val tileMap = parseTiles(input)

    val transformedTileMap = getTileTransformations(tileMap)

    val cornerPieces = findCornerPieces(transformedTileMap)

    val solution = cornerPieces.map(_.toLong).product
    println(s"Solution for ${getCallingMainClass} is: $solution")
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString
    val inputSeaMonster = Helper.source(Some("src/main/resources/day20-sea-monster.txt")).mkString

    val solution = solve(input, inputSeaMonster)
    println(s"Solution for ${getCallingMainClass} is: ${solution.numberOfWaterPixelsNotOccupiedBySeaMonster}")
  }

  def solve(input: String, inputSeaMonster: String): Day20.Part2Result = {
    val transformedTileMap = getTileTransformations(parseTiles(input))

    val allTiles = assembleTiles(transformedTileMap)

    val finalAssembly = assemblePicture(allTiles).withoutBorders
    val seaMonster = parseImage(inputSeaMonster.split("\n").toList)

    val solution = findImageWithSeaMonster(finalAssembly, seaMonster)

    solution
  }
}

object Day20 {

  sealed trait Transformation

  sealed trait Rotation extends Transformation
  case object Clockwise90 extends Rotation
  case object Clockwise180 extends Rotation
  case object Clockwise270 extends Rotation

  sealed trait Flip extends Transformation
  case object FlipVertical extends Flip
  case object FlipHorizontal extends Flip

  case object NoOp extends Transformation

  object Flip {
    val all = List(FlipHorizontal, FlipVertical)
  }

  object Rotation {
    val all = List(Clockwise90, Clockwise180, Clockwise270)
  }

  case class Part2Result(
      transformations: List[Day20.Transformation],
      image: BufferedImage,
      seaMonsterSightings: List[SeaMonsterSighting],
      seaMonsterPixels: Set[(Int, Int)],
      waterPixels: Int,
      numberOfWaterPixelsNotOccupiedBySeaMonster: Int
  )

  case class SeaMonsterSighting(topLeftCoordinates: (Int, Int), allCoordinatesOfSeaMonster: Vector[(Int, Int)])

  def parseTiles(input: String): Map[Int, BufferedImage] = {
    val tiles = input.split("\n\n").map(_.split("\n").toList).toList

    val tileMap = tiles.map(parseTile).toMap

    tileMap
  }

  def getTileTransformations(tileMap: Map[Int, BufferedImage]): Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])] = {
    tileMap.map { case (id, original) =>
      val transformedImages = distinctTransformations.map { transformationList =>
        transformationList -> applyTransformations(original, transformationList)
      }.toMap
      (id, (original, transformedImages))
    }
  }

  case class Edge(top: Int, bottom: Int, left: Int, right: Int) {
    def edges: List[Int] = List(top, right, left, bottom)
  }

  def getEdges(img: BufferedImage): Edge = {
    Edge(
      top(img),
      bottom(img),
      left(img),
      right(img)
    )
  }

  def calcEdges(
      transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]
  ): Map[Int, (BufferedImage, Map[List[Transformation], (BufferedImage, Edge)])] = {
    transformedTileMap.view.mapValues { case (original, transformationMap) =>
      val edgeMap = transformationMap.view.mapValues(img => (img, getEdges(img)))
      original -> edgeMap.toMap
    }.toMap

  }

  def findNeighbors(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): List[List[Int]] = {

    val edges = calcEdges(transformedTileMap)

    val edgeMatches: Map[Int, List[Int]] = edges.toList
      .flatMap { case (id, (_, edgeMap)) =>
        val edgeHashes = edgeMap.values.flatMap(_._2.edges)
        edgeHashes.map(hash => (hash, id))
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2).distinct)
      .toMap

    val matches = edgeMatches.toList.sortBy(_._1)

    val denormalizedMatches = matches.flatMap { case (edgeHash, ids) => ids.map(id => (edgeHash, id)) }.sortBy(_._2)

    val neighbors = denormalizedMatches
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sorted)
      .filter(_._2.size > 1)
      .values
      .toList
      .distinct

    neighbors
  }

  def findCornerPieces(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): List[Int] = {
    findCornerPiecesByNeighbors(findNeighbors(transformedTileMap))
  }
  def findCornerPiecesByNeighbors(neighbors: List[List[Int]]): List[Int] = {

    val sharedEdgesPerTile = neighbors
      .flatMap { ids => ids.map(id => (id, 1)) }
      .groupBy { case (id, _) => id }
      .view
      .mapValues(_.map(_._2).sum)
      .toList

    sharedEdgesPerTile
      .filter(_._2 == 2)
      .map(_._1)

  }

  def applyTransformations(image: BufferedImage, transformationList: List[Transformation]): BufferedImage = {
    transformationList
      .foldLeft(image)((image, op) => applyTransformation(image, op))
  }

  def applyTransformation(image: BufferedImage, transformation: Transformation): BufferedImage = {
    import java.awt.geom.AffineTransform

    val w = image.getWidth
    val h = image.getHeight
    val tpe = image.getType

    val dest = new BufferedImage(w, h, tpe)
    val graphics2D = dest.createGraphics

    val tx = transformation match {
      case NoOp =>
        new AffineTransform()
      case Clockwise90 =>
        AffineTransform.getRotateInstance(math.toRadians(90), image.getWidth / 2, image.getHeight / 2)

      case Clockwise180 =>
        AffineTransform.getRotateInstance(math.toRadians(180), image.getWidth / 2, image.getHeight / 2)

      case Clockwise270 =>
        AffineTransform.getRotateInstance(math.toRadians(270), image.getWidth / 2, image.getHeight / 2)

      case FlipHorizontal =>
        val tx = AffineTransform.getScaleInstance(-1, 1)
        tx.translate(-w, 0)
        tx
      case FlipVertical =>
        val tx = AffineTransform.getScaleInstance(1, -1)
        tx.translate(0, -h)
        tx
    }
    graphics2D.setTransform(tx)
    graphics2D.drawImage(image, 0, 0, null)
    graphics2D.dispose()
    dest
  }

  def parseTile(lines: List[String]): (Int, BufferedImage) = {
    //Tile 2311:
    //..##.#..#.

    val id = lines.head.split(" ").last.dropRight(1).toInt

    val imageLines = lines.tail
    val image = parseImage(imageLines)
    (id, image)
  }

  def parseImage(imageLines: List[String]): BufferedImage = {
    val image = new BufferedImage(imageLines.head.length, imageLines.length, BufferedImage.TYPE_BYTE_BINARY)

    imageLines.zipWithIndex
      .foreach { case (line, y) =>
        line.zipWithIndex.foreach { case (char, x) =>
          val color = if (char == '#') Color.WHITE else Color.BLACK
          image.setRGB(x, y, color.getRGB)
        }
      }

    image

  }

  def cloneImage(image: BufferedImage): BufferedImage = {
    val cm = image.getColorModel
    val raster = image.copyData(null)
    new BufferedImage(cm, raster, false, null)
  }

  val allTransformations: List[List[Transformation]] = {
    Rotation.all.flatMap(rot => Flip.all.map(flip => List(rot, flip))) ++ Flip.all.flatMap(flip => Rotation.all.map(rot => List(flip, rot))) ++ Rotation.all
      .map(List(_)) ++ Flip.all.map(List(_)) ++ List(List(NoOp))
  }

  val distinctTransformations: List[List[Transformation]] = {
    List(
      List(NoOp),
      List(FlipHorizontal),
      List(FlipVertical),
      List(Clockwise90),
      List(Clockwise180),
      List(Clockwise270),
      List(Clockwise90, FlipVertical),
      List(FlipVertical, Clockwise90)
    )
  }

  def top(image: BufferedImage): Int = {
    val coords = 0.until(image.getWidth).map(x => (x, 0))
    getColorArrayHash(image, coords)
  }

  def bottom(image: BufferedImage): Int = {
    val coords = 0.until(image.getWidth).map(x => (x, image.getHeight - 1))
    getColorArrayHash(image, coords)
  }

  def left(image: BufferedImage): Int = {
    val coords = 0.until(image.getHeight).map(y => (0, y))
    getColorArrayHash(image, coords)
  }

  def right(image: BufferedImage): Int = {
    val coords = 0.until(image.getHeight).map(y => (image.getWidth - 1, y))
    getColorArrayHash(image, coords)
  }

  def getColorArrayHash(image: BufferedImage, coords: Seq[(Int, Int)]): Int = {
    val binary = coords.zipWithIndex
      .foldLeft(Array.fill(coords.length)(Color.BLACK)) { case (acc, ((x, y), i)) =>
        acc.updated(i, new Color(image.getRGB(x, y)))
      }
      .map(c => if (c == Color.black) "0" else "1")
      .mkString

    Integer.parseInt(binary, 2)
  }

  def createGraph(neighbors: List[List[Int]]): Graph[Int] = {
    val neighborDiGraph = neighbors
      .flatMap { list =>
        List(list, list.reverse)
      }

    neighborDiGraph.groupBy(_.head).view.mapValues { l => l.map(_.last -> 1).toMap }

  }

  def findPath(from: Int, to: Int, neighbors: List[List[Int]]): Option[List[Int]] = {

    //1951 --> 2311 --> 3079

    findPath(from, to, createGraph(neighbors))
  }

  def findPath(from: Int, to: Int, graph: Graph[Int]): Option[List[Int]] = {
    val result = shortestPath(graph)(from, to)

    result

  }

  def outerEdgesUnsorted(neighbors: List[List[Int]], graph: Graph[Int]): List[(Int, Int, Int, List[Int])] = {

    //find path between two cornerpieces

    val cornerPieces = findCornerPiecesByNeighbors(neighbors)

    val paths = cornerPieces.sorted
      .combinations(2)
      .toList
      .map { case List(c1, c2) =>
        val path = findPath(c1, c2, graph)
        (c1, c2, path.map(_.size).get, path.get)
      }

    //there are 4 edges
    paths.sortBy(_._3).take(4)

  }

  case class Edges(topEdgeLeftToRight: List[Int], leftEdgeTopToBottom: List[Int], rightEdgeTopToBottom: List[Int], bottomEdgeLeftToRight: List[Int])

  def orientEdges(edgesUnsorted: List[(Int, Int, Int, List[Int])]): Edges = {
    val assumedTopEdge :: rest = edgesUnsorted.map(_._4)

    val n = assumedTopEdge.size

    val topLeft = assumedTopEdge.head
    val topRight = assumedTopEdge.last

    val leftEdge = rest.find(_.contains(topLeft)).get
    val rightEdge = rest.find(_.contains(topRight)).get

    val leftEdgeTopToBottom = if (leftEdge.head == topLeft) leftEdge else leftEdge.reverse
    val rightEdgeTopToBottom = if (rightEdge.head == topRight) rightEdge else rightEdge.reverse

    val bottomLeft = (leftEdgeTopToBottom.head :: leftEdgeTopToBottom.last :: Nil).diff(List(topLeft)).head

    val bottomEdge = rest.diff(List(leftEdge, rightEdge)).head
    val bottomEdgeLeftToRight = if (bottomEdge.head == bottomLeft) bottomEdge else bottomEdge.reverse

    Edges(assumedTopEdge, leftEdgeTopToBottom, rightEdgeTopToBottom, bottomEdgeLeftToRight)
  }

  def assembleTiles(
      transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]
  ): Vector[Vector[(Int, List[Transformation], BufferedImage)]] = {

    val neighbors = findNeighbors(transformedTileMap)

    val neighborDiGraph = neighbors
      .flatMap { list =>
        List(list, list.reverse)
      }

    val tilesWithNeighbors = neighborDiGraph
      .groupBy(_.head)
      .map { case (id, group) => (id, group.size, group.map(_.last)) }
      .toList

    val cornerPieces = tilesWithNeighbors
      .filter(_._2 == 2)
      .map(_._1)

    val edges = calcEdges(transformedTileMap)

    val edgeList: List[(Int, Edge, List[Transformation], BufferedImage)] = edges.flatMap { case (id, (_, edgeMap)) =>
      edgeMap.map { case (transformations, (img, edges)) =>
        (id, edges, transformations, img)
      }
    }.toList

    val edgeUsage = edgeList
      .flatMap { case (id, edge, _, _) =>
        edge.edges.map(hash => (hash, id))
      }
      .distinct
      .groupBy(_._1)
      .view
      .map { case (hash, matches) => (hash, matches.size, matches.map(_._2)) }
      .toList

    val outerEdges = edgeUsage
      .filter(_._2 == 1)
      .sortBy(_._1)

    val outerEdgeHashes = outerEdges.map(_._1).toSet

    val outerEdgesPerTile = outerEdges
      .map { case (hash, _, List(id)) => (id, hash) }
      .groupBy(_._1)
      .map { case (id, group) => id -> group.map(_._2) }

    val topLeftCornerPiece = cornerPieces.head
    val outerEdgesOfCornerPiece = outerEdgesPerTile(topLeftCornerPiece).toSet

    val allTransformationsOfCornerPiece = edges(topLeftCornerPiece)._2
    val validTransformationsOfCornerPiece = allTransformationsOfCornerPiece
      .filter { case (_, (_, edge)) =>
        outerEdgesOfCornerPiece.contains(edge.top) && outerEdgesOfCornerPiece.contains(edge.left)
      }

    //just pick the last one - then it matches with the example from AOC
    val topLeftConfiguration: (List[Transformation], (BufferedImage, Edge)) = validTransformationsOfCornerPiece.last

    val (topLeftTransformations, (topLeftImg, topLeftEdge)) = topLeftConfiguration
    val n = math.sqrt(transformedTileMap.size).toInt

    val coords = for {
      y <- 0.until(n).toList
      x <- 0.until(n)
    } yield (x, y)

    type Row = ((Int, Edge, List[Transformation], BufferedImage))

    val assembledTiles = coords
      .foldLeft(Map((0, 0) -> (topLeftCornerPiece, topLeftTransformations, topLeftEdge, topLeftImg))) { case (acc, (x, y)) =>
        if (acc.contains((x, y))) acc
        else {
          val useTopTileFilter = y > 0
          val useLeftTileFilter = x > 0

          val topTileFilter: Row => Boolean = { row =>
            val bottomEdgeOfTopTile = acc((x, y - 1))._3.bottom
            row._2.top == bottomEdgeOfTopTile
          }

          val leftTileFilter: ((Int, Edge, List[Transformation], BufferedImage)) => Boolean = { row =>
            val rightEdgeOfLeftTile = acc((x - 1, y))._3.right
            row._2.left == rightEdgeOfLeftTile
          }

          val leftOuterFilter: Row => Boolean = { row =>
            outerEdgeHashes.contains(row._2.left)
          }

          val topOuterFilter: Row => Boolean = { row =>
            outerEdgeHashes.contains(row._2.top)
          }

          val filterFn: ((Int, Edge, List[Transformation], BufferedImage)) => Boolean = { row =>
            (useTopTileFilter, useLeftTileFilter) match {
              case (true, true)   => topTileFilter(row) && leftTileFilter(row)
              case (true, false)  => topTileFilter(row) && leftOuterFilter(row)
              case (false, true)  => leftTileFilter(row) && topOuterFilter(row)
              case (false, false) => topOuterFilter(row) && leftOuterFilter(row)
            }
          }

          val matchingEdges = edgeList.filter(filterFn).filterNot { case (id, _, _, _) => acc.exists(_._2._1 == id) }
          assert(matchingEdges.size == 1)
          val (id, edges, transformations, image) = matchingEdges.head
          acc.updated((x, y), (id, transformations, edges, image))
        }
      }
      .map { case ((x, y), (id, transformations, _, img)) => ((x, y), (id, transformations, img)) }

    0.until(n)
      .map { y =>
        0.until(n)
          .map { x =>
            assembledTiles((x, y))
          }
          .toVector
      }
      .toVector
  }

  case class FinalImages(withBorders: BufferedImage, imgWithRedRedSeparatorBorders: BufferedImage, withoutBorders: BufferedImage)
  def assemblePicture(tiles: Vector[Vector[(Int, List[Transformation], BufferedImage)]]): FinalImages = {
    val n = tiles.size
    val img1 = tiles.head.head._3

    val imgWithBorders = new BufferedImage(img1.getWidth * n, img1.getHeight * n, img1.getType)
    val rasterWithBorders = imgWithBorders.getRaster

    val imgWithoutBorders = new BufferedImage((img1.getWidth - 2) * n, (img1.getHeight - 2) * n, img1.getType)
    val rasterWithoutBorders = imgWithoutBorders.getRaster

    val imgWithRedBorders = new BufferedImage(imgWithBorders.getWidth, imgWithBorders.getHeight, BufferedImage.TYPE_INT_RGB)
    val rasterWithRedBorders = imgWithRedBorders.getGraphics

    tiles.zipWithIndex.foreach { case (row, y) =>
      row.zipWithIndex.foreach { case ((_, _, img), x) =>
        val xWithBorder = img1.getWidth * x
        val yWithBorder = img1.getHeight * y
        rasterWithBorders.setDataElements(xWithBorder, yWithBorder, img.getRaster)

        val xWithoutBorder = (img1.getWidth - 2) * x
        val yWithoutBorder = (img1.getHeight - 2) * y
        val tileWithoutBorder = img.getSubimage(1, 1, img.getWidth - 2, img.getHeight - 2)
        rasterWithoutBorders.setDataElements(xWithoutBorder, yWithoutBorder, tileWithoutBorder.getRaster)

        rasterWithRedBorders.drawImage(img, xWithBorder, yWithBorder, null)
        0.until(img.getWidth).foreach { dx =>
          0.until(img.getHeight).foreach { dy =>
            if (dx == 0 || dx == img.getWidth - 1 || dy == 0 || dy == img.getHeight - 1) {
              val redX = xWithBorder + dx
              val redY = yWithBorder + dy
              val isBlack = img.getRGB(dx, dy) == Color.BLACK.getRGB
              if (isBlack) {
                imgWithRedBorders.setRGB(redX, redY, Color.RED.getRGB)
              }
            }
          }
        }

      }
    }

    FinalImages(imgWithBorders, imgWithRedBorders, imgWithoutBorders)

  }

  def assembleVectorWithOuterEdges(edges: Edges): Vector[Vector[Option[Int]]] = {

    val Edges(topEdgeLeftToRight, leftEdgeTopToBottom, rightEdgeTopToBottom, bottomEdgeLeftToRight) = edges

    val n = topEdgeLeftToRight.size

    val coordsOfEdges = List(
      topEdgeLeftToRight.zipWithIndex.map { case (id, x) => (x, 0, id) },
      leftEdgeTopToBottom.zipWithIndex.map { case (id, y) => (0, y, id) },
      rightEdgeTopToBottom.zipWithIndex.map { case (id, y) => (n - 1, y, id) },
      bottomEdgeLeftToRight.zipWithIndex.map { case (id, x) => (x, n - 1, id) }
    ).flatten

    val emptyVector: Vector[Vector[Option[Int]]] = Vector.fill(n)(Vector.fill(n)(None))
    val result = coordsOfEdges.foldLeft(emptyVector) { case (acc, (x, y, id)) =>
      val row = acc(y)
      val updatedRow = row.updated(x, Some(id))

      acc.updated(y, updatedRow)
    }

    result
  }

  def findSeaMonsters(image: BufferedImage, seaMonsterImage: BufferedImage): List[SeaMonsterSighting] = {

    //image 24x24
    //sea maonster: 20x3

    val deltaX = image.getWidth - seaMonsterImage.getWidth
    val deltaY = image.getHeight - seaMonsterImage.getHeight

    val startingCoords = 0
      .until(deltaX)
      .flatMap(x =>
        0.until(deltaY)
          .map(y => (x, y))
      )

    val seaMonsterCoords = 0
      .until(seaMonsterImage.getWidth)
      .flatMap(x =>
        0.until(seaMonsterImage.getHeight)
          .map(y => (x, y))
      )
      .filter { case (x, y) =>
        seaMonsterImage.getRGB(x, y) == Color.WHITE.getRGB
      }
      .toVector

    val matchesPerCoordinate = startingCoords.map { case pos @ (xOffset, yOffset) =>
      val absoluteSeaMonsterCoords = seaMonsterCoords.map { case (xSea, ySea) =>
        val x = xSea + xOffset
        val y = ySea + yOffset
        (x, y)
      }
      val cnt = absoluteSeaMonsterCoords.count { case (x, y) =>
        image.getRGB(x, y) == Color.white.getRGB
      }

      (pos, cnt, absoluteSeaMonsterCoords)
    }.toList

    val seaMonsterStartingCoords = matchesPerCoordinate.filter(_._2 == seaMonsterCoords.size)

    seaMonsterStartingCoords.map { case (starting, _, allCoordsOfSeaMonster) => SeaMonsterSighting(starting, allCoordsOfSeaMonster) }
  }

  def findImageWithSeaMonster(finalAssembly: BufferedImage, seaMonster: BufferedImage): Part2Result = {
    val transformedImages = distinctTransformations.map { transformations =>
      (transformations, applyTransformations(finalAssembly, transformations))
    }

    val results = transformedImages.map { case (transformations, img) =>
      (transformations, img, findSeaMonsters(img, seaMonster))
    }

    val validResults = results.filter(_._3.nonEmpty)

    assert(validResults.size == 1)
    val validResult = validResults.head

    val seaMonsterPixels = validResult._3.flatMap(_.allCoordinatesOfSeaMonster).toSet

    val waterPixels = 0
      .until(validResult._2.getWidth)
      .flatMap(x =>
        0.until(validResult._2.getHeight)
          .map(y => (x, y))
      )
      .filter { case (x, y) =>
        validResult._2.getRGB(x, y) == Color.WHITE.getRGB
      }
      .toSet

    val waterPixelsNotOccupiedBySeaMonster = waterPixels.diff(seaMonsterPixels)

    Part2Result(validResult._1, validResult._2, validResult._3, seaMonsterPixels, waterPixels.size, waterPixelsNotOccupiedBySeaMonster.size)

  }

}
