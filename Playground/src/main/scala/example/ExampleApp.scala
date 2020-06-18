package example

import empties._

object ExampleApp {
  def main(args: Array[String]): Unit = {
    val t1 = new NonEmpty(3, Empty, Empty)
    t1.incl(4)
    t1.incl(2)
    println(t1)

    def nth[T](n: Int, xs: List[T]): T =
      if (xs.isEmpty) throw new IndexOutOfBoundsException
      else if (n == 0) xs.head
      else nth(n-1, xs.tail)
  }
}





