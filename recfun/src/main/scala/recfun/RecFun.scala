package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList) )
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println(countChange(4, List(1,2)))

    println(countChange(300,List(5,10,20,50,100,200,500)))


  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceOne(chars: List[Char], opened: Int): Boolean = {
      if (opened < 0) false
      else if (chars.isEmpty) opened == 0
      else if (chars.head == '(') balanceOne(chars.tail, opened+1)
      else if (chars.head == ')') balanceOne(chars.tail, opened-1)
      else balanceOne(chars.tail, opened)
    }
   balanceOne(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else coins.map(coin1 => countChange(money-coin1, coins.filter(coin2 => coin2 >= coin1))).sum
  }
}
