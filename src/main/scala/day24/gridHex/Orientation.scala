package day24.gridHex

import scala.math._

/** Class to define the hex orientation, either flat top or pointy top.
  */
case class Orientation(
    f0: Double,
    f1: Double,
    f2: Double,
    f3: Double,
    b0: Double,
    b1: Double,
    b2: Double,
    b3: Double,
    startAngle: Double
) {
  private val Pi_over_3 = Pi / 3.0
  val offsetX = Array(
    cos(Pi_over_3 * (startAngle - 0)),
    cos(Pi_over_3 * (startAngle - 1)),
    cos(Pi_over_3 * (startAngle - 2)),
    cos(Pi_over_3 * (startAngle - 3)),
    cos(Pi_over_3 * (startAngle - 4)),
    cos(Pi_over_3 * (startAngle - 5))
  )
  val offsetY = Array(
    sin(Pi_over_3 * (startAngle - 0)),
    sin(Pi_over_3 * (startAngle - 1)),
    sin(Pi_over_3 * (startAngle - 2)),
    sin(Pi_over_3 * (startAngle - 3)),
    sin(Pi_over_3 * (startAngle - 4)),
    sin(Pi_over_3 * (startAngle - 5))
  )
}

object Orientation {
  val TOP_FLAT = Orientation(
    f0 = 3.0 / 2.0,
    f1 = 0.0,
    f2 = sqrt(3.0) / 2.0,
    f3 = sqrt(3.0),
    b0 = 2.0 / 3.0,
    b1 = 0.0,
    b2 = -1.0 / 3.0,
    b3 = sqrt(3.0) / 3.0,
    startAngle = 0.0
  )
  val TOP_POINTY = Orientation(
    f0 = sqrt(3.0),
    f1 = sqrt(3.0) / 2.0,
    f2 = 0.0,
    f3 = 3.0 / 2.0,
    b0 = sqrt(3.0) / 3.0,
    b1 = -1.0 / 3.0,
    b2 = 0.0,
    b3 = 2.0 / 3.0,
    startAngle = 0.5
  )
}
