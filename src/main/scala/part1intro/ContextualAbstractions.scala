package part1intro

import scala.concurrent.duration.DurationInt

object ContextualAbstractions {

  // 1 implicit classes
  case class Person(name: String) {
    def greet(): String = s"Hi my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  }

  /// extension method
  val greeting = "Peter".greet() // new ImpersonableString("Peter").greet()

  // example: scala.concurrent.duration._
  val oneSecond = 1.second

  // 2 implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x * amount
  implicit val defaultAmount: Int = 10
  val twelve = increment(2) // implicit argument 10 passed by the compiler

  def multiply(x: Int)(implicit factor: Int) = x * factor
  val aHundred = multiply(10)


  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // SMALL API
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  implicit val personSerializer = new JSONSerializer[Person] {
    override def toJson(person: Person): String = "{\"name\" : \"" + person.name + "\"}"
  }

  val davidsJson = convertToJson(Person("david")) // implicit serializer passed here

  // 3 implicit defs
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}"
    }

  val personsJson = convertToJson(List(Person("Alice"), Person("Bob")))


  // 4 implicit conversions - not recommended
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)
  val aCat: Cat = "Garfield" // string2Cat("Garfield")
  val garfieldMeowing = "Garfield".meow() // string2Cat("Garfield").meow

  def main(args: Array[String]): Unit = {
    println(davidsJson)
    println(personsJson)
  }
}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  // part 2 - type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJSON(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJSON(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJSON(value: Person): String =
      s"""
         | { "name" : "${value.name}", "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertToJSON[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJSON(value)

  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJSON(value)).mkString("[",",","]")


  // part 4 - add extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJSON(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJSON(List(Person("alice", 23), Person("xavier", 45))))
    val bob = Person("bob", 68)
    import JSONSyntax._
    println(bob.toJson)
  }
}