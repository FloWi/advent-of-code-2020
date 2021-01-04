package day23

import helper.Helper._

import scala.annotation.tailrec

object part1 {

  def main(args: Array[String]): Unit = {

    val solution = solve("784235916")

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): String = {
    val initial = parse(input)

    val finalState = play(initial, 100).last._2

    //After the crab is done, what order will the cups be in?
    //Starting after the cup labeled 1, collect the other cups' labels clockwise into a single string with no extra characters;
    //each number except 1 should appear exactly once.

    putThisCupFirst(1, finalState).tail.mkString
  }

  def putThisCupFirst(cupValue: Int, cups: List[Int]): List[Int] = {
    //    Cups:    :List(3, 4, 6, 7, 2, 5, 8, 9, 1)
    //    cupValue: 7             ^
    //    Expected :List(7, 2, 5, 8, 9, 1, 3, 4, 6)

    val indexOfFirst = cups.indexOf(cupValue)

    cups.drop(indexOfFirst) ++ cups.take(indexOfFirst)

    val c = indexOfFirst % cups.length
    cups.drop(c) ++ cups.take(c)
  }

  def parse(input: String): List[Int] = {
    input.toList.map(_.toString.toInt)
  }

  def play(cups: List[Int], numberOfRounds: Int): IndexedSeq[(Int, List[Int])] = {
    val result = 1.to(numberOfRounds).scanLeft((cups.head, cups)) { case ((currentCup, currentCups), _) =>
      playRound(currentCup, currentCups)
    }
    result
  }

  def playRound(currentCup: Int, cups: List[Int]): (Int, List[Int]) = {

    val currentCupIndex = cups.indexOf(currentCup)
    val pickupIndices = 1.to(3).map(i => (currentCupIndex + i) % cups.size)
    val pickup = pickupIndices.map(i => cups(i))
    val newCups = cups.diff(pickup)

    val destinationCup = findDestinationCup(newCups, currentCup - 1)
    val destinationCupIndex = newCups.indexOf(destinationCup) % cups.size
    val (v1, v2) = newCups.splitAt(destinationCupIndex + 1)
    val finalCups = v1 ++ pickup ++ v2

    val indexOfCurrentCupInNewSetup = finalCups.indexOf(currentCup)
    val nextCurrentCup = finalCups((indexOfCurrentCupInNewSetup + 1) % cups.size)

    println(s"\n=======================================")
    println(s"cups: ${cups.zipWithIndex.map { case (c, i) => if (i == currentCupIndex) s"($c)" else s" $c " }.mkString}")
    println(s"current cup: $currentCup")
    println(s"current cup index: $currentCupIndex")
    println(s"pick up: ${pickup.mkString(", ")}")
    println(s"destination: $destinationCup")
    println(s"new cups: $finalCups")
    println(s"indexOfCurrentCupInNewSetup: $indexOfCurrentCupInNewSetup")
    println(s"nextCurrentCup: $nextCurrentCup")

    (nextCurrentCup, finalCups)

  }

  @tailrec
  def findDestinationCup(currentCups: List[Int], candidate: Int): Int = {
    //The crab selects a destination cup:
    //the cup with a label equal to the current cup's label minus one.
    //If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up.
    //If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.

    if (candidate < currentCups.min) currentCups.max
    else if (currentCups.contains(candidate)) candidate
    else findDestinationCup(currentCups, candidate - 1)
  }
}
object Day21 {}
