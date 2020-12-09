package day8

//start: 14:04
//break: 14:11

//start: 18:46
//end:   19:37
// 6 + 51 = 57min

import helper.Helper._
import day8._

object day8 {

  sealed trait Instruction
  case class Accumulator(value: Int) extends Instruction
  case class Jump(value: Int) extends Instruction
  case class NoOp(value: Int) extends Instruction

  def parseInstruction(line: String): Instruction = {

    line
      .split(" ")
      .toList match {
      case List(op, offset) =>
        if (op == "nop") NoOp(offset.toInt)
        else if (op == "acc") Accumulator(offset.toInt)
        else if (op == "jmp") Jump(offset.toInt)
        else throw new RuntimeException(s"unknown op-code: '$op'")

      case _ => throw new RuntimeException(s"can't parse line: '$line'")
    }
  }

  sealed trait ProgramResult
  case class TerminatedSuccessfully(accumulator: Accumulator)
      extends ProgramResult
  case class CancelledDueToEndlessLoop(accumulator: Accumulator)
      extends ProgramResult

  def runInstructions(instructions: Seq[Instruction]): ProgramResult = {
    //Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?
    val vec = instructions.toVector

    def recurse(
        currentInstructionPointer: Int,
        currentAccumulator: Accumulator,
        visitedInstructions: Set[Int]
    ): ProgramResult = {
      if (visitedInstructions.contains(currentInstructionPointer))
        CancelledDueToEndlessLoop(currentAccumulator)
      else if (currentInstructionPointer == vec.size)
        TerminatedSuccessfully(currentAccumulator)
      else {
        val instruction = vec(currentInstructionPointer)
        println(s"""currentInstructionPointer: $currentInstructionPointer
currentAccumulator: $currentAccumulator
instruction: $instruction
visitedInstructions: $visitedInstructions
        """)

        val (nextPointer, nextAcc) = instruction match {
          case Accumulator(value) =>
            (
              currentInstructionPointer + 1,
              Accumulator(currentAccumulator.value + value)
            )
          case Jump(value) =>
            (currentInstructionPointer + value, currentAccumulator)
          case NoOp(_) => (currentInstructionPointer + 1, currentAccumulator)
        }

        recurse(
          nextPointer,
          nextAcc,
          visitedInstructions.+(currentInstructionPointer)
        )
      }
    }

    recurse(0, Accumulator(0), Set.empty)

  }

}

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption).getLines

    val instructions = input.map(parseInstruction)

    val solution = runInstructions(instructions.toList)

    println(s"instructions: \n${instructions.mkString("\n")}")

    println(
      s"Solution for ${getCallingMainClass} is: $solution"
    )
  }

}

object part2 {

  def swap(vector: Vector[Instruction], idx: Int): Instruction = {
    val newOp = vector(idx) match {
      case Jump(value)    => NoOp(value)
      case NoOp(value)    => Jump(value)
      case a: Accumulator => a
    }
    newOp
  }

  def main(args: Array[String]) = {

    val input = source(args.headOption).getLines

    val instructions = input.map(parseInstruction)

    val vec = instructions.toVector
    val indices =
      vec.zipWithIndex.filterNot(_._1.isInstanceOf[Accumulator]).map(_._2)

    val variants = indices.map(i => vec.updated(i, swap(vec, i)))

    val maybeSolution = variants.map(runInstructions).find { result =>
      result.isInstanceOf[TerminatedSuccessfully]
    }

    maybeSolution match {
      case Some(solution) => println(s"Solution: $solution")
      case None           => println("no solution found")
    }

  }

}
