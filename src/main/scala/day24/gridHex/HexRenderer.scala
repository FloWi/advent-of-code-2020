package day24.gridHex

import day24.Day24
import day24.Day24.TileColor

import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D, Polygon, RenderingHints, Stroke}

// https://github.com/mraad/grid-hex
object HexRenderer {
  def render(imgW: Int, imgH: Int, map: Map[Day24.Cube, Day24.TileColor]): BufferedImage = {

    val hexMap = map.map { case (cube, color) =>
      val c = color match {
        case TileColor.White => Color.WHITE
        case TileColor.Black => Color.BLACK
      }
      val hex = Hex.apply(q = cube.x, r = cube.z)
      println(s"conversion $cube, $color, ${hex.show}, $c")
      (hex, c)
    }

    renderHex(imgW, imgH, hexMap)

  }

  def renderHex(imgW: Int, imgH: Int, tiles: Map[Hex, Color]): BufferedImage = {
    val img = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_RGB)

    val sizeX = 50
    val sizeY = 50

    implicit val layout: Layout = Layout(img.getWidth / 2, img.getHeight / 2, sizeX, sizeY, Orientation.TOP_POINTY)

    //val hex = Hex.fromXY(0, 0)

    val g = img.getGraphics.asInstanceOf[Graphics2D]
    try {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
      g.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)

      g.setBackground(Color.WHITE)
      g.clearRect(0, 0, img.getWidth, img.getHeight)

      g.setColor(Color.DARK_GRAY)
      g.drawLine(0, img.getHeight / 2, img.getWidth, img.getHeight / 2)
      g.drawLine(img.getWidth / 2, 0, img.getWidth / 2, img.getHeight)

      def drawPoly(hex: Hex, color: Color): Unit = {

        println(s"drawPoly(${hex.show} will be painted $color")

        val poly = hex.polygon

        val points = poly.toArray
        val xs = points.map(_._1.toInt)
        val ys = points.map(_._2.toInt)

        val xMax = xs.max
        val xMin = xs.min

        val yMax = ys.max
        val yMin = ys.min

        val xMean = (xMax - xMin) / 2 + xMin
        val yMean = (yMax - yMin) / 2 + yMin

        val textColor = if (color == Color.WHITE) Color.BLACK else Color.WHITE

        val polygon = new Polygon(xs, ys, xs.length)
        g.setColor(color)
        g.fillPolygon(polygon)
        g.setColor(Color.darkGray)
        g.drawPolygon(polygon)

        g.setColor(textColor)
        val textSize = g.getFontMetrics.getStringBounds(hex.show, g)
        val textWidth = textSize.getWidth
        val textHeight = textSize.getHeight
        g.drawString(hex.show, (xMean - textWidth / 2).toInt, (yMean + textHeight / 2).toInt)

//        poly
//          .sliding(2)
//          .foreach(pq => {
//            val (px, py) = pq.head
//            val (qx, qy) = pq.last
//            g.setColor(color)
//            g.drawLine(px.toInt, py.toInt, qx.toInt, qy.toInt)
//          })
      }

      g.setColor(Color.BLUE)
      tiles
        .foreach { case (hex, color) => drawPoly(hex, color) }

      //g.fillOval(appParam.x - 6 / 2, appParam.y - 6 / 2, 6, 6)
      //drawPoly(hex, Color.RED)

    } finally {
      g.dispose()
    }
    img
  }

}
