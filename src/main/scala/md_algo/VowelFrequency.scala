package example

object Main extends App {

  /*
  Challenge 5
  Leeeeeeeeeroooooooooy Jeeeeeeeeenkiiiiiiiiins!

  Challenge this time is to make dynamically recreate a Leeroy Jenkins type output.

  Inputs:
  Integer and a String

  Expand or contract the instances of vowels in the String according to the integer values.

  NO VARIABLES OR MUTABLE TYPES

  E.g.
  92
  abracadabra

  aaaaaaaaabraacaaaaaaaaadaabraaaaaaaaa

  Explanation:
  Alternate expanding the vowels by 9 then 2 then 9 then 2 then 9 then 2 etc.
   */

  def challengeFunction(i: Int, s: String): String = {

    val f = (c: Char) => "aeiou".indexOf(c) == -1
    def r(n: Int, s: String, res: String = "", org: Int): String = {
//    assertEquals(mainFunction.challengeFunction(3, "abracadabra"), "aaabraaacaaadaaabraaa")
      if (s.isEmpty) res
      else {
        if (f(s.head)) {
          r(n, s.tail, res + s.head, org)
        } else {
          val rem = n % 10
          val q = n / 10
          val z = s.span(x => !(f(x)))
          if (q == 0)
            r(org, z._2, res + (z._1.distinct * rem), org)
          else r(q, z._2, res + (z._2 * rem), org)
        }
      }
    }

    r(i.toString.reverse.toInt, s, "", i.toString.reverse.toInt)
  }
  val r = challengeFunction(1, "whooooooooooooooooop")
  println(r)
  assert(r == "whop")
}
