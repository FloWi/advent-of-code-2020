package day19

import day19.Day19.{OneAfterTheOtherRule, SingleCharacterRule, _}
import day19.part1.solve
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day19Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
    """.trim
    val actual = solve(input.split("\n").toList)
    val expected = 2

    actual shouldBe expected
  }

  test("parsing SingleCharacterRule") {
    parseRuleLine("""
        |1: "a"
        |""".stripMargin.trim) shouldBe SingleCharacterRule(1, 'a')
  }
  test("parsing OneAfterTheOtherRule") {
    parseRuleLine("""
        |0: 1 2
        |""".stripMargin.trim) shouldBe OneAfterTheOtherRule(0, List(1, 2))
  }
  test("parsing OrRule") {
    // 1: 2 3 | 3 2
    // 28: 104 | 95

    parseRuleLine("""
        |1: 2 3 | 3 2
        |""".stripMargin.trim) shouldBe OrRule(1, List(2, 3), List(3, 2))
  }
}

class Day19Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input =
      """
42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".trim
    val actual = part2.solve(input.split("\n").toList)
    val expected = 12

    actual shouldBe expected
  }
}
