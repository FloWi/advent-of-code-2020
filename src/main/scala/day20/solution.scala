package day20

import day20.Day20.{findCornerPieces, getTileTransformations, parseTiles}
import day20.Dijkstra.{Graph, shortestPath}
import helper.Helper._

import java.awt.Color
import java.awt.image.BufferedImage
import scala.collection.MapView

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

  def parseTiles(input: String): Map[Int, BufferedImage] = {
    val tiles = input.split("\n\n").map(_.split("\n").toList).toList

    val tileMap = tiles.map(parseImage).toMap

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

  def getEdges(img: BufferedImage): List[Int] = {
    List(
      top(img),
      bottom(img),
      left(img),
      right(img)
    )
  }

  def calcEdges(
      transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]
  ): Map[Int, (BufferedImage, Map[List[Transformation], (BufferedImage, List[Int])])] = {
    transformedTileMap.view.mapValues { case (original, transformationMap) =>
      val edgeMap = transformationMap.view.mapValues(img => (img, getEdges(img)))
      original -> edgeMap.toMap
    }.toMap

  }

  def findNeighbors(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): List[List[Int]] = {

    val edges = calcEdges(transformedTileMap)

    val edgeMatches: Map[Int, List[Int]] = edges.toList
      .flatMap { case (id, (_, edgeMap)) =>
        val edgeHashes = edgeMap.values.flatMap(_._2)
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

  def parseImage(lines: List[String]): (Int, BufferedImage) = {
    //Tile 2311:
    //..##.#..#.

    val id = lines.head.split(" ").last.dropRight(1).toInt

    val imageLines = lines.tail
    val image = new BufferedImage(imageLines.length, imageLines.length, BufferedImage.TYPE_BYTE_BINARY)

    imageLines.zipWithIndex
      .foreach { case (line, y) =>
        line.zipWithIndex.foreach { case (char, x) =>
          val color = if (char == '.') Color.BLACK else Color.WHITE
          image.setRGB(x, y, color.getRGB)
        }
      }

    (id, image)
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

  def assemblePicture(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): Vector[Vector[Int]] = {
    val neighbors = findNeighbors(transformedTileMap)

    val graph = createGraph(neighbors)

    val edges = orientEdges(outerEdgesUnsorted(neighbors, graph))
    val pictureOfOuterEdges = assembleVectorWithOuterEdges(edges)

    val rowsStartAndEnd = edges.leftEdgeTopToBottom.zip(edges.rightEdgeTopToBottom).zipWithIndex.map(_.swap)
    val pathsFromLeftToRight = rowsStartAndEnd.flatMap { case (y, (startTile, endTile)) =>
      val path = findPath(startTile, endTile, graph)
      path.get.zipWithIndex.map { case (id, x) => (x, y, id) }
    }

    pathsFromLeftToRight
      .foldLeft(pictureOfOuterEdges) { case (pic, (x, y, id)) =>
        pic(y)(x) match {
          case Some(oldId) =>
            //this tile has already been set - just validate
            assert(oldId == id)
            pic
          case None =>
            val row = pic(y)
            pic.updated(y, row.updated(x, Some(id)))
        }
      }
      .map(_.map(_.get))
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

}
