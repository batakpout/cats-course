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

    val f = (c: Char) => "AEIOUaeiou".indexOf(c) == -1
    val o = i.toString.reverse.toInt
    def p(n: Int, s: String, r: String = ""): String = {
      if (s.isEmpty) r
      else {
        val h = s.head
        if (f(h)) p(n, s.tail, r + h)
        else {
          val m = n % 10
          val q = n / 10
          val z = s.span(x => x.toLower == h.toLower)
          if (q == 0)
            p(o, z._2, r + h.toString * m)
          else p(q, z._2, r + h.toString * m)
        }
      }
    }
    p(o, s)
  }
}
