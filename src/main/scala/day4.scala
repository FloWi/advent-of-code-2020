import scala.io.Source
import java.nio.file.Paths

package day4 {

  import Helper._
  object part1 {

    object Passport {
      def parse(str: String): Map[String, String] = {
        str
          .split("\n")
          .foldLeft(Map.empty[String, String]) { (map, str) =>
            val List(k, v) = str.split(":").toList
            map.updated(k, v)
          }
      }

      def isValid(map: Map[String, String]): Boolean = {
        val requiredFields =
          Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
        (requiredFields.diff(map.keySet)).size == 0
      }

    }

    def main(args: Array[String]) = {

      val normalized =
        source(args.headOption)
          .getLines()
          .flatMap(_.split(" "))
          .mkString("\n")

      println(normalized)

      val validPassports = normalized
        .split("\n\n")
        .map(Passport.parse)
        .filter(Passport.isValid)

      println(validPassports)
      println(
        s"Solution for ${getCallingMainClass.getCanonicalName}: number of valid passports: ${validPassports.size}"
      )
    }
  }

  object part2 {

    object Passport {
      def parse(str: String): Map[String, String] = {
        str
          .split("\n")
          .foldLeft(Map.empty[String, String]) { (map, str) =>
            val List(k, v) = str.split(":").toList
            map.updated(k, v)
          }
      }

      def isValid(map: Map[String, String]): Boolean = {
        val requiredFields =
          Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
        val requiredFieldsValid = (requiredFields.diff(map.keySet)).size == 0
        val fieldRulesValid = validateFields(map)
        val result = requiredFieldsValid && fieldRulesValid

//         println(s"""
// ########################
// passport:
// ${map.mkString("\n")}
// requiredFieldsValid: $requiredFieldsValid
// fieldRulesValid: $fieldRulesValid
// result: $result
//       """)
        result

      }

      def validateHeight(str: String): Boolean = {
        /* hgt (Height) - a number followed by either cm or in:
           If cm, the number must be at least 150 and at most 193.
           If in, the number must be at least 59 and at most 76.
         */
        val regex = raw"(\d+)(cm|in)".r
        regex.findFirstMatchIn(str) match {
          case Some(value) =>
            val unit = value.group(2)
            val height = value.group(1).toInt
            if (unit == "cm") height >= 150 && height <= 193
            else if (unit == "in") height >= 59 && height <= 76
            else false
          case None => false
        }
      }

      val rules: Map[String, String => Boolean] = Map(
        "byr" -> { str => str.toInt >= 1920 && str.toInt <= 2002 },
        "iyr" -> { str => str.toInt >= 2010 && str.toInt <= 2020 },
        "eyr" -> { str => str.toInt >= 2020 && str.toInt <= 2030 },
        "hgt" -> { validateHeight },
        "hcl" -> (raw"#[a-f0-9]{6}".r).matches,
        "ecl" -> Set(
          "amb",
          "blu",
          "brn",
          "gry",
          "grn",
          "hzl",
          "oth"
        ).contains,
        "pid" -> (raw"\d{9}".r).matches
      )

      def validateFields(map: Map[String, String]): Boolean = {
        /*
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
         */
        val results = map.map { case (key, value) =>
          val result = rules.get(key).map(rule => rule(value))
          (key, (value, result))
        }
        val result = results.forall(_._2._2.getOrElse(true))
        // println(results.mkString("\n"))
        // println(s"result: $result")
        result

      }
    }

    def main(args: Array[String]) = {

      val normalized =
        source(args.headOption)
          .getLines()
          .flatMap(_.split(" "))
          .mkString("\n")

      println(normalized)

      val parsed = normalized
        .split("\n\n")
        .map(Passport.parse)

      val validPassports = parsed
        .filter(Passport.isValid)

      println(s"parsedPassports: ${parsed.size}")
      println(s"validPassports: ${validPassports.size}")
      println(
        s"Solution for ${getCallingMainClass.getCanonicalName}: number of valid passports: ${validPassports.size}"
      )
    }
  }

}
