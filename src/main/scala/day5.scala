package day5
import scala.io.Source
import java.nio.file.Paths
import scala.collection.immutable.Nil

import helper.Helper._

object part1 {

  import day5._

  def main(args: Array[String]) = {

    val seats =
      source(args.headOption)
        .getLines()
        .toList
        .map(input => (input, Seat.parseBinary(input, 128)))

    println(s"found ${seats.size} seats")
    // seats
    //   .foreach(println)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: highest seatId: ${seats.maxBy(_._2.seatId)._2.seatId}"
    )
    val sorted = seats.sortBy { case (_, s) => -s.seatId }

    // sorted.foreach(println)
  }
}

object part2 {

  import day5._

  def main(args: Array[String]) = {

    val seats =
      source(args.headOption)
        .getLines()
        .toList
        .map(input => (input, Seat.parseBinary(input, 128)))

    val minSeatId = seats.map(_._2.seatId).min
    seats
      .map(_._2)
      .sortBy(_.seatId)
      .sliding(2, 1)
      .collect { case List(prev, current) =>
        (
          prev,
          current,
          prev.seatId,
          current.seatId,
          current.seatId - prev.seatId
        )
      }
      .filter(_._5 > 1)
      .foreach { tup =>
        val (_, _, prevId, currentId, _) = tup

        println(tup)
        println(
          s"Solution for ${getCallingMainClass.getCanonicalName}: my seat id is: ${(currentId - prevId) / 2 + prevId}"
        )

      }
  }
}

object day5 {

  case class Seat(row: Int, col: Int) {
    val seatId = row * 8 + col

    override def toString =
      s"Seat(row=${row}, col=${col}, seatId=${seatId})"
  }
  object Seat {

    /*
      The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.

      For example, consider just the first seven characters of FBFBBFFRLR:

      Start by considering the whole range, rows 0 through 127.
      F means to take the lower half, keeping rows 0 through 63.
      B means to take the upper half, keeping rows 32 through 63.
      F means to take the lower half, keeping rows 32 through 47.
      B means to take the upper half, keeping rows 40 through 47.
      B keeps rows 44 through 47.
      F keeps rows 44 through 45.
      The final F keeps the lower of the two, row 44.
      The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.

      For example, consider just the last 3 characters of FBFBBFFRLR:

      Start by considering the whole range, columns 0 through 7.
      R means to take the upper half, keeping columns 4 through 7.
      L means to take the lower half, keeping columns 4 through 5.
      The final R keeps the upper of the two, column 5.
      So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

     */
    def parseBinary(str: String, numberOfRows: Int): Seat = {

      val rowStr = str.take(7).map(c => if (c == 'F') '0' else '1')
      val colStr = str.takeRight(3).map(c => if (c == 'L') '0' else '1')

      Seat(
        row = Integer.parseInt(rowStr, 2),
        col = Integer.parseInt(colStr, 2)
      )
    }
  }
}
