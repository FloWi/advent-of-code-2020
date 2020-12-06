import scala.io.Source
import java.nio.file.Paths
import scala.collection.immutable.Nil
import scala.io.BufferedSource

package day6 {

  import Helper._

  object part1 {

    def main(args: Array[String]) = {

      val input = source(args.headOption).mkString
      //will default to
      //val input = source(Some("src/main/resources/day6.txt")).mkString

      val solution = input
        .split("\n\n")
        .map(_.strip.toSet.filterNot(_.isWhitespace))
        .map(_.size)
        .sum

      // println(input)

      println(
        s"Solution for ${getCallingMainClass.getCanonicalName} is: $solution"
      )
    }
  }

  object part2 {

    def main(args: Array[String]) = {

      val input = source(args.headOption).mkString

      val solution = input
        .split("\n\n")
        .map { groupStr =>
          (
            groupStr,
            groupStr.linesIterator.size,
            groupStr.toList
              .filterNot(_.isWhitespace)
              .groupBy(identity)
              .mapValues(_.size)
          )
        }
        .map { case (groupStr, numberOfMembers, answers) =>
          val numberOfQuestionsAnsweredByAll =
            answers.count(_._2 == numberOfMembers)
          // println(
          //   s"numberOfMembers: $numberOfMembers; answers: ${answers.mkString(",")}; numberOfQuestionsAnsweredByAll: $numberOfQuestionsAnsweredByAll"
          // )
          numberOfQuestionsAnsweredByAll
        }
        .sum

      println(
        s"Solution for ${getCallingMainClass.getCanonicalName} is: $solution"
      )
    }
  }
}
