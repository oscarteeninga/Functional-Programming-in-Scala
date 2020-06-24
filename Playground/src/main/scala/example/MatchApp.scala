package example

object MatchApp extends App {
  trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }

    def show: String = this match {
      case Number(n) => n.toString()
      case Sum(e1, e2) => e1.show + " + " + e2.show
    }
  }
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr

  object Number {
    def apply(n: Int): Number = new Number(n)
  }

  object Sum {
    def apply(e1: Expr, e2: Expr): Sum = new Sum(e1, e2)
  }


  println(Sum(Number(2), Number(2)).eval)

  println(Sum(Number(2), Sum(Number(2), Number(3))).show)

}
