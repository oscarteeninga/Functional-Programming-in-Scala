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

//  def mergesort(xs: List[Int]): List[Int] = {
//    val n = xs.length/2
//    if (n == 0) xs
//    else {
//      def merge(xs: List[Int], ys: List[Int]): List[Int] = {
//        if (xs.isEmpty) ys
//        else if (ys.isEmpty) xs
//        else if (xs.head > ys.head) ys.head :: merge(xs, ys.tail)
//        else xs.head :: merge(xs.tail, ys)
//      }
//      val (fst, snd) = xs.splitAt(n)
//      merge(mergesort(fst), mergesort(snd))
//    }
//  }

  val pair = ("hello", 120)
  val (label, value) = pair

  val tuple = ("hello", 120, 16e-20)
  val (n1, n2, n3) = tuple
  val n4 = tuple._1



//  def mergesort[T](xs: List[T])(lt: (T, T) =>  Boolean): List[T] = {
//    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
//      case (Nil, _ :: _) => ys
//      case (_ :: _, Nil) => xs
//      case (z :: zs, t :: ts) => if (lt(z,t)) t :: merge(xs, ts) else z :: merge(zs, ys)
//    }
//    val n = xs.length/2
//    if (n == 0) xs
//    else {
//      val (fst, snd) = xs.splitAt(n)
//      merge(mergesort(fst), mergesort(snd))
//    }
//  }

  def mergesort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, Nil) => Nil
      case (Nil, _ :: _) => ys
      case (_ :: _, Nil) => xs
      case (z :: zs, t :: ts) => if (ord.lt(z, t)) t :: merge(xs, ts) else z :: merge(zs, ys)
    }
    val n = xs.length/2
    if (n == 0) xs
    else {
      val (fst, snd) = xs.splitAt(n)
      merge(mergesort(fst), mergesort(snd))
    }
  }

//  def squareList(xs: List[Int]): List[Int] = xs match {
//    case Nil => xs
//    case y :: ys => y*y :: squareList(ys)
//  }

  def squareList(xs: List[Int]): List[Int] = xs.map(n => n*n)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case y :: _ => {
      val (e1, e2) = xs.span(x => x == y)
      e1 :: pack(e2)
    }
  }

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(t => (t.head, t.length))


  println(pack(List('a', 'a', 'b', 'b', 'a', 'a', 'a')))

  var time = System.nanoTime()
  println(getLength(list))
  println("Time: " + (System.nanoTime() - time))
  time = System.nanoTime()
  println(list.length)
  println("Time: " + (System.nanoTime() - time))

}
