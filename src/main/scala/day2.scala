package day2
import scala.io.Source
import java.nio.file.Paths

import helper.Helper._

object part1 {

  case class PasswordInstruction(
      min: Int,
      max: Int,
      ruleChar: Char,
      pwd: String
  )
  object PasswordInstruction {
    def parse(str: String) = {
      //1-3 a: abcde
      //For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
      val List(minMax, ruleString, pwd) = str.split(" ").toList
      val List(min, max) = minMax.split("-").toList
      PasswordInstruction(min.toInt, max.toInt, ruleString.head, pwd)
    }

    def isValid(pwd: PasswordInstruction): Boolean = {
      val numberOfChars =
        pwd.pwd.toList.groupBy(identity).toMap.view.mapValues(_.size)

      val numOccurrences = numberOfChars.getOrElse(pwd.ruleChar, 0)
      pwd.min <= numOccurrences && pwd.max >= numOccurrences
    }
  }

  implicit val myClass: Class[_] = getClass()

  def main(args: Array[String]) = {

    val resultList = source(args.headOption)
      .getLines()
      .map(PasswordInstruction.parse)
      .map(pwd => (pwd, PasswordInstruction.isValid(pwd)))

    val result = resultList
      .filter(_._2)
      .size

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $result valid passwords"
    )
  }
}

object day2Part2 {

  case class PasswordInstruction(
      firstLocation: Int,
      secondLocation: Int,
      ruleChar: Char,
      pwd: String
  )
  object PasswordInstruction {
    def parse(str: String) = {
      // locations are 1-indexed
      // 1-3 a: abcde
      // is valid: position 1 contains a and position 3 does not.
      val List(minMax, ruleString, pwd) = str.split(" ").toList
      val List(fst, snd) = minMax.split("-").toList
      PasswordInstruction(fst.toInt, snd.toInt, ruleString.head, pwd)
    }

    def isValid(pwd: PasswordInstruction): Boolean = {

      val firstMatches = pwd.pwd(pwd.firstLocation - 1) == pwd.ruleChar
      val secondtMatches = pwd.pwd(pwd.secondLocation - 1) == pwd.ruleChar

      firstMatches ^ secondtMatches //XOR

    }
  }

  def main(args: Array[String]) = {
    /*
    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.
     */

    implicit val myClass: Class[_] = getClass()

    val resultList =
      source(args.headOption)
        .getLines()
        .map(PasswordInstruction.parse)
        .map(pwd => (pwd, PasswordInstruction.isValid(pwd)))

    val result = resultList
      .filter(_._2)
      .size

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $result valid passwords"
    )
  }
}
