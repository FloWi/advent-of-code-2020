package day20

import helper.Helper._

import java.awt.Color
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val solution = "not yet" //solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {
    ???
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

  def findCornerPieces(transformedTileMap: Map[Int, (BufferedImage, Map[List[Transformation], BufferedImage])]): Unit = {
    val result: List[(Int, Int, Int, List[Transformation])] =
      transformedTileMap.keys.toList.sorted
        .combinations(2)
        .toList
        .flatMap { case List(id1, id2) =>
          val tile1 = transformedTileMap(id1)
          val tile2 = transformedTileMap(id2)

          val tile1Top = top(tile1._1)
          val tile1Bottom = bottom(tile1._1)
          val tile1Left = left(tile1._1)
          val tile1Right = right(tile1._1)

          tile2._2.flatMap { case (t, img2Transformed) =>
            val tile2Top = top(img2Transformed)
            val tile2Bottom = bottom(img2Transformed)
            val tile2Left = left(img2Transformed)
            val tile2Right = right(img2Transformed)

            val matchingEdge: Option[Int] = {
              if (tile1Top == tile2Bottom)
                Some(tile1Top)
              else if (tile1Bottom == tile2Top)
                Some(tile1Bottom)
              else if (tile1Left == tile2Right)
                Some(tile1Left)
              else if (tile1Right == tile2Left)
                Some(tile1Right)
              else None
            }

            matchingEdge.map(e => (e, id1, id2, t))
          }
        }
  }

  def applyTransformations(image: BufferedImage, transformationList: List[Transformation]): BufferedImage = {
    transformationList
      .foldLeft(image)((image, op) => applyTransformation(image, op))
  }

  def applyTransformation(image: BufferedImage, transformation: Transformation): BufferedImage = {
    import java.awt.geom.AffineTransform
    import java.awt.image.AffineTransformOp

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

      case FlipVertical =>
        val tx = AffineTransform.getScaleInstance(-1, 1)
        tx.translate(-w, 0)
        tx
      case FlipHorizontal =>
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
    Rotation.all.flatMap(rot => Flip.all.map(flip => List(rot, flip))) ++ Flip.all.flatMap(flip => Rotation.all.map(rot => List(flip, rot)))
  }

  val distinctTransformations: List[List[Transformation]] = {
    List(
      List(NoOp),
      List(FlipHorizontal),
      List(FlipVertical),
      List(Clockwise90),
      List(Clockwise180),
      List(Clockwise270),
      List(Clockwise90, FlipHorizontal),
      List(Clockwise90, FlipVertical),
      List(Clockwise180, FlipHorizontal),
      List(Clockwise180, FlipVertical),
      List(Clockwise270, FlipHorizontal),
      List(Clockwise270, FlipVertical),
      List(FlipHorizontal, Clockwise90),
      List(FlipHorizontal, Clockwise180),
      List(FlipVertical, Clockwise180),
      List(FlipVertical, Clockwise270)
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

}
