package example


object Main extends App {
  var anyList: List[Any] = List(1, "siema", List(1, 0))

  def show(list: List[Any]): Unit = {
    if (!list.isEmpty) {
      println(list.head)
      show(list.tail)
    }
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (y >= x) x :: xs else y :: insert(x, ys)
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  var list: List[Int] = List(5,4,2,6,7,9,4,5,7,2,5,7,3,4,6)

  show(isort(list))
}
