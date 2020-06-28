package example

object ListApp extends App {
  var list = genList(200).reverse


  def genList(size: Int): List[Int] = {
    if (size == 0) List()
    else size :: genList(size-1)
  }

  def getLength(list: List[Any]): Int = {
    var i = 0
    var tmp = list
    while (!tmp.isEmpty) { tmp = tmp.tail; i += 1 }
    i
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error()
    case List(x) => x
    case _ :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error()
    case List(_) => Nil
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => throw new Error("No such element in list")
    case y :: ys => if (n == 0) ys else y :: removeAt(ys, n-1)
  }

  def flatten(xs: List[Any]): List[Any] = if (xs.nonEmpty) xs.head match {
    case List() => flatten(xs.tail)
    case y :: ys => y match {
      case List() => flatten(ys) ::: flatten(xs.tail)
      case z :: zs => flatten(z :: zs) ::: flatten(ys) ::: flatten(xs.tail)
      case z => z :: flatten(ys) ::: flatten(xs.tail)
    }
    case x => x :: flatten(xs.tail)
  } else List()

  println(flatten(List(1, List(2, 3), List(List(4, 5), 6), List(List(List(7))))))

  var time = System.nanoTime()
  println(getLength(list))
  println("Time: " + (System.nanoTime() - time))
  time = System.nanoTime()
  println(list.length)
  println("Time: " + (System.nanoTime() - time))

  println((1 to 20) filter (_ > 10))


}
