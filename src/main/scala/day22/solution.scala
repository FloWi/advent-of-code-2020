package day22

import day22.Day22.{GameState, findScoreOfWinningDeck}
import helper.Helper._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object part1 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): Long = {
    val initialState = Day22.parse(input)

    val finalState = play(initialState)

    findScoreOfWinningDeck(finalState)
  }

  @tailrec
  def play(gameState: GameState): GameState = {
    val newState = playRound(gameState)

    if (newState.round == gameState.round) gameState else play(newState)
  }

  def playRound(gameState: GameState) = {
    val GameState(round, game, p1Cards, p2Cards) = gameState

    if (p1Cards.isEmpty || p2Cards.isEmpty)
      gameState
    else {
      val (p1Card, newP1Queue) = p1Cards.dequeue
      val (p2Card, newP2Queue) = p2Cards.dequeue

      val playedCards = List(p1Card, p2Card).sorted.reverse

      if (p1Card > p2Card) {
        GameState(round + 1, game, newP1Queue.enqueueAll(playedCards), newP2Queue)
      } else {
        GameState(round + 1, game, newP1Queue, newP2Queue.enqueueAll(playedCards))
      }
    }
  }
}

object part2 {

  def main(args: Array[String]): Unit = {

    val input = source(args.headOption).mkString

    val solution = solve(input)

    println(s"Solution for ${getCallingMainClass} is: $solution")
  }

  def solve(input: String): Long = {
    val initialState = Day22.parse(input)
    val finalState = play(initialState)
    findScoreOfWinningDeck(finalState.gameState)
  }

  val isDebug = false

  def debug(x: Any): Unit = {
    if (isDebug) {
      println(x)
    }
  }

  sealed trait Win {
    def gameState: GameState
  }
  case class Player1Wins(gameState: GameState) extends Win
  case class Player2Wins(gameState: GameState) extends Win

  def play(gameState: GameState, previousState: Set[(Queue[Int], Queue[Int])] = Set.empty): Win = {

    val thisGameKey = (gameState.p1Cards, gameState.p2Cards)

    if (previousState.contains(thisGameKey)) {
      Player1Wins(gameState)
    } else {
      val GameState(round, game, p1Cards, p2Cards) = gameState

      if (p1Cards.isEmpty)
        Player2Wins(gameState)
      else if (p2Cards.isEmpty)
        Player1Wins(gameState)
      else {
        val (p1Card, newP1Queue) = p1Cards.dequeue
        val (p2Card, newP2Queue) = p2Cards.dequeue

        // If both players have at least as many cards remaining in their deck as the value of the card they just drew,
        // the winner of the round is determined by playing a new game of Recursive Combat (see below).
        if (p1Card <= newP1Queue.size && p2Card <= newP2Queue.size) {

          val subgameId = game + 1
          play(GameState(0, subgameId, newP1Queue.take(p1Card), newP2Queue.take(p2Card))) match {
            case Player1Wins(gameState) =>
              play(GameState(round + 1, game, newP1Queue.enqueueAll(List(p1Card, p2Card)), newP2Queue), previousState.+(thisGameKey))
            case Player2Wins(gameState) =>
              play(GameState(round + 1, game, newP1Queue, newP2Queue.enqueueAll(List(p2Card, p1Card))), previousState.+(thisGameKey))
          }

        } else {

          val playedCards = List(p1Card, p2Card).sorted.reverse

          if (p1Card > p2Card) {
            play(GameState(round + 1, game, newP1Queue.enqueueAll(playedCards), newP2Queue), previousState.+(thisGameKey))
          } else {
            play(GameState(round + 1, game, newP1Queue, newP2Queue.enqueueAll(playedCards)), previousState.+(thisGameKey))
          }
        }
      }
    }
  }

}

object Day22 {

  case class GameState(round: Int, game: Int, p1Cards: Queue[Int], p2Cards: Queue[Int])

  def parse(input: String): GameState = {
    val List(p1String, p2String) = input.split("\n\n").toList

    GameState(
      round = 0,
      game = 0,
      p1Cards = Queue.from(
        p1String
          .split("\n")
          .drop(1)
          .map(_.toInt)
      ),
      p2Cards = Queue.from(
        p2String
          .split("\n")
          .drop(1)
          .map(_.toInt)
      )
    )
  }

  def findScoreOfWinningDeck(finalState: GameState): Long = {
    val winningDeck = if (finalState.p1Cards.isEmpty) finalState.p2Cards else finalState.p1Cards
    winningDeck.reverse.zipWithIndex.map { case (card, idx) => card * (idx + 1) }.sum

  }

}
