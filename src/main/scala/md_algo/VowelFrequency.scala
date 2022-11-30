package example

object Main extends App {

   def challengeFunction(i: Int, s: String): String = {

    val f = (c: Char) => "AEIOUaeiou".indexOf(c) == -1
    type S = String
    val o = i.toString.reverse.toInt
    def p(n: Int, s: S, r: S = ""): S = {
      if (s.isEmpty) r
      else {
        val h = s(0)
        if (f(h)) p(n, s.tail, r + h)
        else {
          val m = n % 10
          val z = s.span(x => x.toLower == h.toLower)
          val j = r + h.toString * m
          if (n / 10 == 0)
            p(o, z._2, j)
          else p(n / 10, z._2, j)
        }
      }
    }
    p(o, s)
  }
}
