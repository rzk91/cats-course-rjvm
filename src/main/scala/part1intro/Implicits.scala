package part1intro

object Implicits {

  case class Person(name: String)
  case class Dog(name: String, breed: String)

  trait JsonSerializer[A] {
    def toJson(value: A): String
  }

  def listToJson[A](list: List[A])(implicit serialzier: JsonSerializer[A]): String =
    list.map(serialzier.toJson).mkString("[", ",", "]")

  // Does not restrict to one-argument case classes
  implicit def oneArgCaseClassSerializer[A <: Product]: JsonSerializer[A] = (a: A) =>
    s"""{"${a.productElementName(0)}": "${a.productElement(0)}"}"""

  implicit val personSerializer: JsonSerializer[Person] = (person: Person) =>
    s"""{"name": "${person.name}"}"""

  val personsJson: String = listToJson(List(Person("A"), Person("B")))

  val dogsJson: String = listToJson(List(Dog("Snowy", "Greyhound"), Dog("Murphy", "Greyhound"))) // Should NOT compile!

  def main(args: Array[String]): Unit = {
    println(personsJson)
    println(dogsJson)
  }

}
