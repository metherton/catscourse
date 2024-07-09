package part2AbstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // so that we can import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative

  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = {
//    list.foldLeft(/* WHAT ? */)(_ |+| _)
//  }

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension method for Monoids - |+|
  //import cats.syntax.monoid._ // either this one or cat.syntax.semigroup
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO: 1 implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = {
    list.foldLeft(monoid.empty)(_ |+| _)
  }

  // TODO: 2 combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid, use an import
  val phonebooks = List(
    Map("alice" -> 235, "bob" -> 647),
    Map("charlie" -> 372, "daniel" -> 889),
    Map("tina" -> 123),

  )

  // TODO 3 : SHopping cart and online stores with monoids
  // hint - define your monoid - Monoid.instance
  // hint 2 - use combine by fold
  case class ShoppingCart(items: List[String], total: Double)

  implicit val scMonoid: Monoid[ShoppingCart] = Monoid.instance(ShoppingCart(List(), 0), (sc1: ShoppingCart, sc2: ShoppingCart) => {
    ShoppingCart(sc1.items ++ sc2.items, sc1.total + sc2.total)
  })

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = {
    combineFold(shoppingCarts)
  }

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(List("I ","like ", "monoids")))
    import cats.instances.map._
    println(combineFold(phonebooks))
    val sc1 = ShoppingCart(List("book", "pen"), 45)
    val sc2 = ShoppingCart(List("bike", "computer"), 350)
    val sc3 = ShoppingCart(List("hat", "trousers"), 56)
    println(checkout(List(sc1, sc2, sc3)))
  }
}
