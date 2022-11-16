	object NumberToString extends App {



		def challengeFunction(i: Int) = {
			val b = 1000000000
			val m = 1000000
			val t = 1000
			val h = 100

			def g(n:Int, b:Int, t: String) = {
				val m=n%b
				val d=n/b
				if (m>h) s"${f(d)} $t, ${f(m,false)}"
				else if(m<h && m>0) s"${f(d)} $t and ${f(m,false)}"
				else s"${f(d)} $t ${f(m,false)}"
			}

			def f(n: Int, p: Boolean = true): String = {
				if (n>=b) g(n, b, "billion")
				else if (n>=m) g(n, m, "million")
				else if (n>=t) g(n, t, "thousand")
				else if (n>=h) {
					val m=n%h
					val d=n/h
					if(m>0) s"${f(d)} hundred and ${f(m,false)}"
					else s"${f(d)} hundred ${f(m,false)}"
				}
				else if (n>=20) n/10 match {
					case 2 => s"twenty ${f(n % 10,false)}"
					case 3 => s"thirty ${f(n % 10,false)}"
					case 5 => s"fifty ${f(n % 10,false)}"
					case r@_ => s"${f(r).stripSuffix("t")}ty ${f(n % 10,false)}"
				}
				else {
					n match {
						case 0 => if (p) "zero" else ""
						case 1 => "one"
						case 2 => "two"
						case 3 => "three"
						case 4 => "four"
						case 5 => "five"
						case 6 => "six"
						case 7 => "seven"
						case 8 => "eight"
						case 9 => "nine"
						case 10 => "ten"
						case 11 => "eleven"
						case 12 => "twelve"
						case 13 => "thirteen"
						case 15 => "fifteen"
						case n@_ =>
							s"${f(n - 10).stripSuffix("t")}teen"
					}
				}
			}

			val s=f(i)
			(s.substring(0, 1).toUpperCase() + s.substring(1))
				.replaceAll("( )+", " ")

		}
		println("=========")

		println(challengeFunction(7361529).replaceAll("( )+", " "))
		println("=========")


		println(challengeFunction(46))
		println("=========")
		println(challengeFunction(234))
		println("=========")
		println(challengeFunction(95716))
		println("=========")
		println(challengeFunction(95716))
		println("=========")
		println(challengeFunction(1000000))
		println("=========")
		println(challengeFunction(1812613414))
		println("=========")
		println(challengeFunction(1019035949))
		println("=========")
		println(challengeFunction(6101))
		println("=========")

		println(challengeFunction(1000001))
		println("=========")

		println(challengeFunction(6100))
	}
