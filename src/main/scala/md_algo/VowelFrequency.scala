package example

object Main extends App {

   def challengeFunction(i: Int, s: String): String = {
    type S = String
    val o = i.toString.reverse.toInt
    def p(n: Int, s: S, r: S = ""): S = {
      if (s == "") r
      else {
        val h = s(0)
        if ("AEIOUaeiou".indexOf(h) == -1) p(n, s.tail, r + h)
        else {
          val z = s.span(_.toLower == h.toLower)
          val j = r + h.toString * (n % 10)
          if (n / 10 == 0) p(o, z._2, j) else p(n / 10, z._2, j)
        }
      }
    }
    p(o, s)
  }
}
