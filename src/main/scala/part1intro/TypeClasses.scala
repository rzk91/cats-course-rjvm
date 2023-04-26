package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  trait JsonSerializer[A] {
    def toJson(value: A): String
  }

  // Implicit type classes
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = s""""$value""""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {

    override def toJson(value: Person): String =
      s"""
         |{ "name" : "${value.name}", "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // Serialization API
  def convertListToJson[A](list: List[A])(implicit serializer: JsonSerializer[A]): String =
    list.map(serializer.toJson).mkString("[", ", ", "]")

  // Extending existing types
  object JsonSyntax {

    implicit class JsonOps[A](private val value: A)(implicit serializer: JsonSerializer[A]) {
      def toJson: String = serializer.toJson(value)
    }

    implicit class JsonListOps[A](private val list: List[A])(implicit serializer: JsonSerializer[A]) {
      def toJson: String = list.map(serializer.toJson).mkString("[", ", ", "]")
    }
  }

  def main(args: Array[String]): Unit = {
    import JsonSyntax._
    println("One".toJson)
    println(Person("Snowy", 5).toJson)
    println(List(1, 2, 3, 4).toJson)
  }
}
