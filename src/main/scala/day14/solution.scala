package day14

// start: 17:39
//   end: 18:46
//      =  1:07h

import helper.Helper._

import scala.annotation.tailrec

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    @tailrec
    def helper(restLines: List[String], currentMask: Option[String], currentMemory: Map[Long, Long]): Map[Long, Long] = {
      restLines match {
        case Nil => currentMemory
        case ::(head, tail) =>
          val List(leftPart, rightPart) = head.split(" = ").toList.map(_.trim)
          if (leftPart == "mask") {
            helper(tail, Some(rightPart), currentMemory)
          } else if (leftPart.startsWith("mem")) {
            currentMask match {
              case None => throw new RuntimeException("no mask defined")
              case Some(mask) =>
                val memoryAddress = leftPart.replace("mem[", "").replace("]", "").toInt
                val value = rightPart.toInt
                val maskedValue = maskValue(mask, value)
                helper(tail, currentMask, currentMemory.updated(memoryAddress, maskedValue))
            }
          } else {
            throw new RuntimeException("unknown state")
          }
      }
    }

    val result = helper(lines, None, Map.empty)

    result.values.sum

  }

  def maskValue(mask: String, value: Long): Long = {
    // The current bitmask is applied to values immediately before they are written to memory:
    // a 0 or 1 overwrites the corresponding bit in the value, while an X leaves the bit in the value unchanged.
    val maskLength = mask.length
    val binaryString = ("0" * maskLength + value.toBinaryString).takeRight(maskLength)

    val maskedValueString = mask
      .zip(binaryString)
      .foldLeft("") { case (acc, (maskChar, valueChar)) =>
        if (maskChar == '1' || maskChar == '0')
          acc + maskChar
        else acc + valueChar
      }

    java.lang.Long.parseLong(maskedValueString, 2)
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).getLines
      .filterNot(_.isEmpty)
      .toList

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(lines: List[String]): Long = {

    @tailrec
    def helper(restLines: List[String], currentMask: Option[String], currentMemory: Map[Long, Long]): Map[Long, Long] = {
      restLines match {
        case Nil => currentMemory
        case ::(head, tail) =>
          val List(leftPart, rightPart) = head.split(" = ").toList.map(_.trim)
          if (leftPart == "mask") {
            helper(tail, Some(rightPart), currentMemory)
          } else if (leftPart.startsWith("mem")) {
            currentMask match {
              case None => throw new RuntimeException("no mask defined")
              case Some(mask) =>
                val memoryAddress = leftPart.replace("mem[", "").replace("]", "").toInt
                val maskedMemoryAddresses = getMaskedMemoryAddresses(mask, memoryAddress)
                val value = rightPart.toInt
                val updatedMemory = maskedMemoryAddresses.foldLeft(currentMemory)((mem, address) => mem.updated(address, value))
                helper(tail, currentMask, updatedMemory)
            }
          } else {
            throw new RuntimeException("unknown state")
          }
      }
    }

    val result = helper(lines, None, Map.empty)

    result.values.sum

  }

  def getMaskedMemoryAddresses(mask: String, value: Long): List[Long] = {
    // The current bitmask is applied to values immediately before they are written to memory:
    // a 0 or 1 overwrites the corresponding bit in the value, while an X leaves the bit in the value unchanged.
    val maskLength = mask.length
    val binaryString = ("0" * maskLength + value.toBinaryString).takeRight(maskLength)

    val maskedValueStrings = mask
      .zip(binaryString)
      .foldLeft(List("")) { case (acc, (maskChar, valueChar)) =>
        if (maskChar == '0') {
          acc.map(_ + valueChar)
        } else if (maskChar == '1') {
          acc.map(_ + maskChar)
        } else {
          acc.flatMap { str =>
            List('0', '1').map { charToAdd =>
              str + charToAdd
            }
          }
        }
      }

    maskedValueStrings.map(java.lang.Long.parseLong(_, 2))
  }
}
