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
    val b = i.toString.reverse.toInt
    def r(n: Int, s: String, q: String = "", o: Int): String = {
      val h = s.head
      if (s.isEmpty) q
      else {
        if (f(h)) {
          r(n, s.tail, q + h, o)
        } else {
          val e = n % 10
          val q = n / 10
          val z = s.span(x => !f(x))
          if (q == 0)
            r(o, z._2, q + (z._1.distinct * e), o)
          else r(q, z._2, q + (z._2 * e), o)
        }
      }
    }
    r(b, s, "", b)
  }
  val r = challengeFunction(1, "whooooooooooooooooop")
  println(r)
  assert(r == "whop")
}
