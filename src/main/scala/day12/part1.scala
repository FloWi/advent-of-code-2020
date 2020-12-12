package day12

// part 1
// start: 13:39
//   end: 15:25
//      =  1:46h

import helper.Helper._

object part1 {

  import Day12._

  case class State(position: Position, direction: Direction)

  case class Position(north: Int, east: Int)

  type Direction = Int

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(Action.parse)

    input.foreach(println)

    val initial = State(Position(0, 0), 0)
    val results = input.scanLeft(initial) { case (current, action) =>
      applyAction(current, action)
    }

    results.foreach(println)
    val lastLocation = results.last.position
    val solution = math.abs(lastLocation.north) + math.abs(lastLocation.east)
    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  //ship starts facing east
  //Action N means to move north by the given value.
  //Action S means to move south by the given value.
  //Action E means to move east by the given value.
  //Action W means to move west by the given value.
  //Action L means to turn left the given number of degrees.
  //Action R means to turn right the given number of degrees.
  //Action F means to move forward by the given value in the direction the ship is currently facing.

  def applyAction(state: State, action: Action): State = {
    val pos = state.position
    val direction = state.direction
    action match {
      case North(value) =>
        state.copy(position = pos.copy(north = pos.north + value))
      case South(value) =>
        state.copy(position = pos.copy(north = pos.north - value))
      case East(value) =>
        state.copy(position = pos.copy(east = pos.east + value))
      case West(value) =>
        state.copy(position = pos.copy(east = pos.east - value))
      case Left(value) =>
        state.copy(direction = (direction - value + 360) % 360)
      case Right(value) =>
        state.copy(direction = (direction + value + 360) % 360)
      case Forward(value) =>
        direction match {
          case 0   => applyAction(state, East(value))
          case 90  => applyAction(state, South(value))
          case 180 => applyAction(state, West(value))
          case 270 => applyAction(state, North(value))
        }
    }
  }
}

object part2 {

  import Day12._

  case class State(
      waypoint: Position,
      position: Position
  ) {
    override def toString: String = s"waypoint: $waypoint; position: $position"

  }

  case class Position(north: Int, east: Int) {
    override def toString: String = s"(east: $east; north: $north)"
  }

  type Direction = Int

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(Action.parse)

    val initial = State(
      waypoint = Position(north = 1, east = 10),
      position = Position(north = 0, east = 0)
    )

    def testAssert(
        testWaypoint: Position,
        rotationDegrees: Int,
        expected: Position
    ) = {
      val actual =
        rotate(testWaypoint, rotationDegrees) //R90 --> clockwise is negative
      assert(
        actual == expected,
        s"rotate $testWaypoint R90 --> expected: $expected ; actual: $actual"
      )
    }

    testAssert(Position(north = 4, east = 10), -90, Position(north = -10, east = 4))
    testAssert(Position(east = 1, north = -10), -90, Position(east = -10, north = -1))
    testAssert(Position(east = -1, north = 10), 180, Position(east = 1, north = -10))

    val results = input.scanLeft(initial) { case (current, action) =>
      val newState = applyAction(current, action)
      println(s"action: ${showAction(action)}; $newState")
      newState
    }

    val lastLocation = results.last.position
    val solution = math.abs(lastLocation.north) + math.abs(lastLocation.east)
    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  //ship starts facing east
  //Action N means to move north by the given value.
  //Action S means to move south by the given value.
  //Action E means to move east by the given value.
  //Action W means to move west by the given value.
  //Action L means to turn left the given number of degrees.
  //Action R means to turn right the given number of degrees.
  //Action F means to move forward by the given value in the direction the ship is currently facing.

  def showAction(action: Action): String = {
    s"${action.getClass.getName.split("\\$")(1).head}${action.toString.split("\\(")(1).dropRight(1)}"
  }

  def applyAction(state: State, action: Action): State = {
    val pos = state.position
    val waypoint = state.waypoint
    action match {
      case North(value) =>
        state.copy(waypoint = waypoint.copy(north = waypoint.north + value))
      case South(value) =>
        state.copy(waypoint = waypoint.copy(north = waypoint.north - value))
      case East(value) =>
        state.copy(waypoint = waypoint.copy(east = waypoint.east + value))
      case West(value) =>
        state.copy(waypoint = waypoint.copy(east = waypoint.east - value))
      case Left(value) =>
        //Rotate the waypoint around the ship left (counter-clockwise), but don't change the position of the ship
        state.copy(waypoint = rotate(waypoint, value))
      case Right(value) =>
        //Rotate the waypoint around the ship right (clockwise), but don't change the position of the ship
        state.copy(waypoint = rotate(waypoint, -value))
      case Forward(value) =>
        val offset =
          Position(north = waypoint.north * value, east = waypoint.east * value)
        state.copy(position =
          Position(
            north = pos.north + offset.north,
            east = pos.east + offset.east
          )
        )
    }
  }

  def rotate(position: Position, angleInDeg: Int): Position = {

    // https://stackoverflow.com/questions/12161277/how-to-rotate-a-vertex-around-a-certain-point
    // had a copy & waste error and needed to debug my soltion using the result of Sophie Alpert's solution in python. So a bit cheating involved.

    // a better way to rotate point would be this (since we're only having to deal with multiples of 90°)
    // https://calcworkshop.com/transformations/rotation-rules/
    // 180°   : A(x,y) --> A'(-x,-y)
    // 90° CCW: A(x,y) --> A'(-y,x)
    // 90°  CW: A(x,y) --> A'(y,-x)

    val rad = math.toRadians(angleInDeg)
    val newX = position.east * math.cos(rad) - position.north * math.sin(rad)
    val newY = position.east * math.sin(rad) + position.north * math.cos(rad)

    Position(north = newY.round.toInt, east = newX.round.toInt)
  }
}

object Day12 {

  sealed trait Action

  case class North(value: Int) extends Action

  case class South(value: Int) extends Action

  case class East(value: Int) extends Action

  case class West(value: Int) extends Action

  case class Left(value: Int) extends Action

  case class Right(value: Int) extends Action

  case class Forward(value: Int) extends Action

  object Action {
    def parse(str: String): Action = {
      val head = str.head
      val value = str.drop(1).toInt

      head match {
        case 'N' => North(value)
        case 'S' => South(value)
        case 'E' => East(value)
        case 'W' => West(value)
        case 'L' =>
          if (math.abs(value) % 90 != 0)
            throw new RuntimeException("rotations only in 90° steps")
          Left(value)
        case 'R' =>
          if (math.abs(value) % 90 != 0)
            throw new RuntimeException("rotations only in 90° steps")
          Right(value)
        case 'F' => Forward(value)
      }
    }

  }

}
