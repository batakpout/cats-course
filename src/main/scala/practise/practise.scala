package practise

object semigroup extends App {


	 import cats.Semigroup
	 import cats.instances.int._

	 val naturalIntSG = Semigroup[Int]
	 val intCombo = naturalIntSG.combine(1, 2)
	 println(s"intCombo: $intCombo")

	 import cats.instances.string._
	 val naturalStringSG = Semigroup[String]
	 val stringCombo = naturalStringSG.combine("hi ", "there")
	 println(stringCombo)

	def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSG.combine)
	def reduceStrings(list: List[String]): String = list.reduce(naturalStringSG.combine)

	def genericReduce[A](list: List[A])(implicit semiGroup: Semigroup[A]):A = {
		list.reduce(semiGroup.combine)
	}
	println(genericReduce(List(1,2,3,4,5)))
	println(genericReduce(List("Hi ", "How ", "are ", "you?")))

	case class Expense(id: Long, amount: Double)

	implicit val expenseSemiGroup: Semigroup[Expense] = Semigroup.instance { (exp1, exp2) =>
		Expense(Math.max(exp1.id , exp2.id), exp1.amount + exp2.amount)
	}
	println(genericReduce(List(Expense(1, 20.2), Expense(2, 12.2))))

	import cats.syntax.semigroup._
	val resul1 = 1 |+| 2
	println(resul1)
	val resul2 = "Hi " |+| "there"
	println(resul2)
	val resul3 = Expense(12, 121.2) |+| Expense(3, 24.2)
	println(resul3)

	//def genericReduceOps[A](list: List[A])(implicit sg: Semigroup[A]): A = list.reduce(_ |+| _)
	def genericReduceOps[A : Semigroup](list: List[A]): A = list.reduce(_ |+| _)
	println(genericReduceOps(List(Expense(100, 20.2), Expense(2, 12.2))))

	import cats.instances.option._

	val result4 = Option(10) |+| None |+| Option(2) |+| None
	println(result4)
}

object monoid extends App {


    import cats.Semigroup
	  import cats.instances.int._
		import cats.syntax.semigroup._

	(1 to 100).toList.fold(0)(_ + _)

/*	def combineFold[A : Semigroup](list: List[A]): A = {
		list.foldLeft(/*What*/)(_ |+| _)
	}*/

	import cats.Monoid
	val intMonoid = Monoid[Int]
	val combineInt = intMonoid.combine(20, 10)
	println(combineInt)
	val identityElement: Int = intMonoid.empty
	println(identityElement)

	val stringMonoid = Monoid[String]
	val combineString = stringMonoid.combine("hello ", "there..")
	println(combineString)
	val stringIdentityElement = stringMonoid.empty
	println(stringIdentityElement)

	def combineFold[A](list: List[A])(implicit monoid: Monoid[A]):A = {
		list.foldLeft(monoid.empty)(_ |+| _)
	}

	import cats.instances.string._
	println(combineFold(List(1,2,3)))
	println(combineFold(List("Hi ", "there ", " man..")))

	val phonebooks = List(
		Map(
			"Alice" -> 235,
			"Bob" -> 647
		),
		Map(
			"Charlie" -> 372,
			"Daniel" -> 889
		),
		Map(
			"Tina" -> 123
		)
	)

	import cats.instances.map._
	println(combineFold(phonebooks))

	case class ShoppingCart(items: List[String], total: Double)

	implicit val shoppingCartMonoidInstance = Monoid.instance[ShoppingCart](ShoppingCart(Nil, 0.0), (sc1, sc2)  =>
		ShoppingCart(sc1.items ++ sc2.items, sc1.total + sc2.total)
	)

	def checkout(shoppingCarts: List[ShoppingCart]) = combineFold(shoppingCarts)

	println {
		checkout(List(ShoppingCart(List("bag", "shoes"), 1000), ShoppingCart(List("laptop", "books"), 60000)))
	}



}
