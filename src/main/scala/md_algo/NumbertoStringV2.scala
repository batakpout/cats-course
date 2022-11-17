def challengeFunction(i: Int): String = {
  val t = 1000
  val h = 100
  val b = t * t * t
  val m = b/t
  val x = Seq(
    "",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
  )

  val c = Seq("","","twenty ","thirty ","forty ","fifty ")
  def g(n: Int, b: Int, t: String) = {
    val m = n % b
    val d = f(n / b)
    if (m > h) s"$d $t, ${f(m)}"
    else if (m < h && m > 0) s"$d $t and ${f(m)}"
    else s"$d $t ${f(m)}"
  }

  def f(n: Int): String = {
    if (n >= b) g(n, b, "billion")
    else if (n >= m) g(n, m, "million")
    else if (n >= t) g(n, t, "thousand")
    else if (n >= h) {
      val d = f(n / h)
      (if (n % h > 0) d + " hundred and " else d + " hundred ") + f(n % h)
    } else if (n >= 20) {
      val j = n / 10
      (if (j < 6) c(j)
       else f(j).stripSuffix("t") + "ty ") + f(n % 10)
    } else if (n >=0 && n < 16) x(n)
    else f(n - 10).stripSuffix("t") + "teen"
  }
  val s = f(i)
  s(0).toUpper + s.tail
    .replaceAll(" +", " ").trim
}
