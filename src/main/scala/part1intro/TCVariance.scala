package part1intro

object TCVariance {
  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  // Because Eq[Int] and Eq[Option[Int]] TC instances have been either directly/indirectly imported
  val optionComparison: Boolean = Option(2) === Option(3)
  // val anInvalidComparison: Boolean = Some(2) === None
  // Doesn't work because Eq[Some[Int]] not found; only Eq[Option[Int]] is found
  // Variance issue!

  // Variance
  class Animal
  class Cat extends Animal

  // Covariant type: subtyping is propagated to generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // Contravariant type: subtyping is propagated backwards to generic type
  // Mainly used for action types
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, so Vet[Cat] >: Vet[Animal]

  // Rule of thumb
  // If a generic type "HAS a T" -> covariant
  // If a generic type "ACTS on T" -> contravariant
  // So, in the above cage and vet examples:
  // If I want a cage to HAVE any animal; a cage that HAS a cat works fine
  // If I want a vet to ACT on a cat; a vet that can ACT on all animals works fine, too

  // Variance affects how TC instances are fetched
  // CONTRAVARIANT example
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  // Implementation not important
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("woof!")
  makeSound[Animal] // OK - TC instance defined above
  makeSound[Cat]    // OK (???) - TC instance for Animal is also applicable for Cats
  // Rule 1: Contravariant TCs can use superclass instances if nothing is available for that type specifically

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]] // This works because SoundMaker is contravariant!
  // Doesn't work for Eq because Eq is invariant!

  // COVARIANT example
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GenericAnimalShow extends AnimalShow[Animal] {
    def show: String = "Animal everywhere!"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    def show: String = "MIAAAAOOOWW"
  }

  def organiseShow[T](implicit event: AnimalShow[T]): String = event.show
  // Rule 2: Covariant TCs will always use the more-specific TC instance for that type
  // If the generic TC instance is also present in scope, the compiler will get confused and will hence not compile!
  // If only a specific TC instance is in scope, the compiler will use that for the more-generic type
  // So, here, it'll use AnimalShow[Cat] for organiseShow[Animal] if AnimalShow[Animal] is not present in scope

  // Rule 3: You can NOT have both co- and contravariance for the same TC
  // Cats use INVARIANT TCs! So...
  Option(2) === Option.empty[Int] // Compiles because they're both of type Option

  def main(args: Array[String]): Unit = {
    println(organiseShow[Cat])    // OK - compiler injects CatsShow as implicit
//    println(organiseShow[Animal]) // Does NOT compile because:
    // ambiguous implicit values:
    // both object GenericAnimalShow in object TCVariance of type part1intro.TCVariance.GenericAnimalShow.type
    // and object CatsShow in object TCVariance of type part1intro.TCVariance.CatsShow.type
  }

}
