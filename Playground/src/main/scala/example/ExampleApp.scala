package example

object ExampleApp extends App {
  var person: Person = new Person(150, 70)
  var integer: Int = 10
  var string: String = "Hejka"
  person.addToMap(integer, string)
  println(person.getFromMap(integer))

}
