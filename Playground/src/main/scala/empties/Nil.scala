package empties

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil has no head")
  override def tail: Nothing = throw new NoSuchElementException("Nil has no tail")
}
