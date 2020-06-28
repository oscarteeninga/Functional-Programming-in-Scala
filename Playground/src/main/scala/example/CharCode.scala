package example

object CharCode {

  val words = List()

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  // Pairs [A-Z] -> [2-9]
  var charCode: Map[Char, Char] = {
    for ((digit, str) <- mnem; char <- str) yield char -> digit
  }

  def wordCode(word: String): String = word.toUpperCase map charCode

  def wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.nonEmpty) {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet else Set(List())


  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

}
