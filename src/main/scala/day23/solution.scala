package day23

import day23.Day23._
import helper.Helper._

object part1 {

  def main(args: Array[String]): Unit = {

    val solution = solve("784235916")

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): String = {
    val initial = parse(input)

    val finalState = play(initial, 100)

    //After the crab is done, what order will the cups be in?
    //Starting after the cup labeled 1, collect the other cups' labels clockwise into a single string with no extra characters;
    //each number except 1 should appear exactly once.

    val numbers = finalState.take(finalState.size - 1, finalState.find(1).map(_.next)).get._2.map(_.value)
    numbers.mkString
  }

}

object part2 {

  def main(args: Array[String]): Unit = {

    val solution = solve("784235916")

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }
  def solve(input: String): Long = {
    val cups = parse(input)
    val initialElements = cups.allElementsHeadToTail.map(_.value)
    val rest = (initialElements.max + 1).to(1000 * 1000)
    rest.foreach(cups.append)

    val finalState = play(cups, 10 * 1000 * 1000)

    //After the crab is done, what order will the cups be in?
    //Starting after the cup labeled 1, collect the other cups' labels clockwise into a single string with no extra characters;
    //each number except 1 should appear exactly once.

    val numbers = finalState.take(n = 2, entrypoint = finalState.find(1).map(_.next)).get._2.map(_.value.toLong)
    val result = numbers.product
    println(s"two numbers clockwise of 1 are: ${numbers.mkString(", ")}")
    println(s"result: $result")
    result
  }

}
object Day23 {

  def parse(input: String): CircularList[Int] = {
    val cupsList = CircularList.create[Int]()

    input.toList
      .map(_.toString.toInt)
      .foreach(cupsList.append)

    cupsList
  }

  def debugPlay(cups: CircularList[Int], numberOfRounds: Int): IndexedSeq[(Int, List[Int])] = {

    val result = 1.to(numberOfRounds).scanLeft((cups.tail.get.next.value, cups.allElementsHeadToTail.map(_.value))) { case ((currentCup, _), _) =>
      val (nextCurrentCup, _) = playRound(currentCup, cups)
      (nextCurrentCup, cups.allElementsHeadToTail.map(_.value))
    }
    result
  }

  def play(cups: CircularList[Int], numberOfRounds: Int): CircularList[Int] = {
    val _ = 1.to(numberOfRounds).foldLeft(cups.tail.get.next.value) { case (currentCup, _) => playRound(currentCup, cups)._1 }

    cups
  }

  def playRound(currentCup: Int, cups: CircularList[Int]): (Int, CircularList[Int]) = {

    //    val initialCups = cups.allElementsHeadToTail.map(_.value)

    val totalNumberOfCups: Int = cups.size
    val pickup = cups.unlinkAfter(currentCup, 3)
    //    val pickedUpElements = pickup.allElementsHeadToTail.map(_.value)

    val destinationCup = findDestinationCup(cups, currentCup - 1, totalNumberOfCups)
    cups.linkAfter(destinationCup, pickup)

    //    val indexOfCurrentCupInNewSetup = finalCups.indexOf(currentCup)
    //    val nextCurrentCup = finalCups((indexOfCurrentCupInNewSetup + 1) % cups.size)

    val nextCurrentCup = cups.find(currentCup).get.next.value

    //    println(s"\n=======================================")
    //    println(s"cups: ${initialCups.map { c => if (c == currentCup) s"($c)" else s" $c " }.mkString}")
    //    println(s"current cup: $currentCup")
    //    println(s"pick up: ${pickedUpElements.mkString(", ")}")
    //    println(s"destination: $destinationCup")
    //    println(s"new cups: ${cups.allElementsHeadToTail.map(_.value)}")
    //    println(s"nextCurrentCup: $nextCurrentCup")

    (nextCurrentCup, cups)
  }

  def findDestinationCup(currentCups: CircularList[Int], candidate: Int, totalNumberOfCups: Int): Int = {

    val rangeToPickFrom = if (candidate < 3) candidate.to(1, -1) ++ totalNumberOfCups.to(totalNumberOfCups - 3, -1) else candidate.to(candidate - 3, -1)

    //The crab selects a destination cup:
    //the cup with a label equal to the current cup's label minus one.
    //If this would select one of the cups that was just picked up, the crab will keep subtracting one until it finds a cup that wasn't just picked up.
    //If at any point in this process the value goes below the lowest value on any cup's label, it wraps around to the highest value on any cup's label instead.

    rangeToPickFrom.find(currentCups.contains).get
  }

}
