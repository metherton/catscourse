package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - create implicit type class instances
  implicit object StringSerializer extends JsonSerializer[String] {

    override def toJson(value: String): String = "\"" + value + "\""
  }
  implicit object IntSerializer extends JsonSerializer[Int] {

    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {

    override def toJson(value: Person): String =
      s"""
        |{ "name" : ${value.name}, "age" : ${value.age} }
        |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Xavier", 45))))
    val bob = Person("Bob", 35)
    import JSONSyntax._
    println(bob.toJson)
  }
}
