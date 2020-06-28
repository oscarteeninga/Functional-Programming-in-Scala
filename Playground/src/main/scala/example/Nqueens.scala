package example

object Nqueens extends App {
  def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row-1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => c != col && math.abs(col - c) != row - r
      }
    }
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
    }
    placeQueens(n)
  }

  def showBoard(queen: List[Int]): Unit = {
    for {
      col <- queen.reverse
    } { println(); queen.indices foreach (n => if (n == col) print(" Q") else print(" *" )) }
    println()
  }
  val queen = queens(4)
  println(queen.size)
  queens(4) map showBoard
}
