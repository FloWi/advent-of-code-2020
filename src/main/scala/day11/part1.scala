package day11

// part 1
// start: 12:23
//   end: 13:37
//      = 1:04h
import helper.Helper._

import scala.annotation.tailrec

object day11 {}

object part1 {

  def main(args: Array[String]): Unit = {

    val input: Seats = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(_.toVector)

    val solution = solvePart1(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  type Seats = Vector[Vector[Char]]

  def solvePart1(seats: Seats): Int = {
    // . floor
    // L empty seat
    // # occupied seat

    //left, right, up, down and diagonal

    val offsets: Vector[Offset] = (for {
      x <- (-1).to(1)
      y <- (-1).to(1)
      if (x, y) != (0, 0)
    } yield Offset(x, y)).toVector

    val positions: Vector[Position] = (for {
      y <- seats.indices
      x <- seats(y).indices
    } yield Position(x, y)).toVector

    @tailrec
    def helper(currentSeats: Seats): Seats = {
      val newSeats = calcNewSeatOccupancy(currentSeats, offsets, positions)
      println("===================== new state =====================")
      newSeats.foreach(row => println(row.mkString("")))

      if (newSeats == currentSeats) {
        println("\n YAY - seat layout didn't change. Final state")
        newSeats
      } else {
        println("\n seat layout changed. One more try...")
        helper(newSeats)
      }
    }

    println("===================== initial state =====================")
    seats.foreach(row => println(row.mkString("")))

    val latest = helper(seats)
    latest.flatten
      .count(_ == '#')
  }

  def calcNewSeatOccupancy(
      seats: Seats,
      offsets: Vector[Offset],
      positions: Vector[Position]
  ): Seats = {

    positions.foldLeft(seats) { case (currentSeats, pos @ Position(x, y)) =>
      val newSeat = calcNewSeat(seats, offsets, pos)
      currentSeats.updated(y, currentSeats(y).updated(x, newSeat))
    }

  }

  def calcNewSeat(
      seats: Seats,
      offsets: Vector[Offset],
      position: Position
  ): Char = {

    //    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    //    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    //    Otherwise, the seat's state does not change.

    val seat = seats(position.y)(position.x)
    val isEmpty = seat == 'L'
    val isOccupied = seat == '#'
    val isFloor = seat == '.'

    if (isEmpty) {
      if (numberOfAdjacentSeatsOccupied(seats, offsets, position) == 0) '#'
      else seat
    } else if (isOccupied) {
      if (numberOfAdjacentSeatsOccupied(seats, offsets, position) >= 4) 'L'
      else seat
    } else if (isFloor) {
      seat
    } else seat
  }

  case class Offset(x: Int, y: Int)
  case class Position(x: Int, y: Int) {
    def atOffset(offset: Offset): Position =
      Position(x + offset.x, y + offset.y)
  }

  def numberOfAdjacentSeatsOccupied(
      seats: Seats,
      offsets: Vector[Offset],
      pos: Position
  ): Int = {

    offsets
      .map(pos.atOffset)
      .filterNot { case Position(x, y) =>
        x < 0 || y < 0 || y >= seats.size || x >= seats.head.size
      }
      .count { case Position(x, y) => seats(y)(x) == '#' }
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input: Seats = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toVector
      .map(_.toVector)

    val solution = solvePart2(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  type Seats = Vector[Vector[Char]]

  def solvePart2(seats: Seats): Int = {
    // . floor
    // L empty seat
    // # occupied seat

    //left, right, up, down and diagonal

    val directions: Vector[Offset] = (for {
      x <- (-1).to(1)
      y <- (-1).to(1)
      if (x, y) != (0, 0)
    } yield Offset(x, y)).toVector

    val height = seats.size
    val width = seats.head.size

    val directionOffsets: Vector[Vector[Offset]] = directions.map { case Offset(x, y) =>
      1.to(math.max(height, width)).map(o => Offset(x * o, y * o)).toVector
    }

    val positions: Vector[Position] = (for {
      y <- seats.indices
      x <- seats(y).indices
    } yield Position(x, y)).toVector

    @tailrec
    def helper(currentSeats: Seats): Seats = {
      val newSeats =
        calcNewSeatOccupancy(currentSeats, directionOffsets, positions)
      println("===================== new state =====================")
      newSeats.foreach(row => println(row.mkString("")))

      if (newSeats == currentSeats) {
        println("\n YAY - seat layout didn't change. Final state")
        newSeats
      } else {
        println("\n seat layout changed. One more try...")
        helper(newSeats)
      }
    }

    println("===================== initial state =====================")
    seats.foreach(row => println(row.mkString("")))

    val latest = helper(seats)
    latest.flatten
      .count(_ == '#')
  }

  def calcNewSeatOccupancy(
      seats: Seats,
      offsets: Vector[Vector[Offset]],
      positions: Vector[Position]
  ): Seats = {

    positions.foldLeft(seats) { case (currentSeats, pos @ Position(x, y)) =>
      val newSeat = calcNewSeat(seats, offsets, pos)
      currentSeats.updated(y, currentSeats(y).updated(x, newSeat))
    }

  }

  def calcNewSeat(
      seats: Seats,
      offsets: Vector[Vector[Offset]],
      position: Position
  ): Char = {

    //    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    //    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    //    Otherwise, the seat's state does not change.

    val seat = seats(position.y)(position.x)
    val isEmpty = seat == 'L'
    val isOccupied = seat == '#'
    val isFloor = seat == '.'

    if (isEmpty) {
      if (numberOfVisibleSeatsOccupied(seats, offsets, position) == 0) '#'
      else seat
    } else if (isOccupied) {
      if (numberOfVisibleSeatsOccupied(seats, offsets, position) >= 5) 'L'
      else seat
    } else if (isFloor) {
      seat
    } else seat
  }

  case class Offset(x: Int, y: Int)
  case class Position(x: Int, y: Int) {
    def atOffset(offset: Offset): Position =
      Position(x + offset.x, y + offset.y)
  }

  def numberOfVisibleSeatsOccupied(
      seats: Seats,
      directionOffsets: Vector[Vector[Offset]],
      pos: Position
  ): Int = {

    directionOffsets.map { offsets =>
      val positionsToCheckInsideMap = offsets
        .map(pos.atOffset)
        .filterNot { case Position(x, y) =>
          x < 0 || y < 0 || y >= seats.size || x >= seats.head.size
        }
      val seatsInsideMap = positionsToCheckInsideMap
        .find { case Position(x, y) =>
          val seat = seats(y)(x)
          seat == '#' || seat == 'L'
        }
      seatsInsideMap match {
        case Some(Position(x, y)) if seats(y)(x) == '#' => 1
        case _                                          => 0
      }

    }.sum
  }
}
