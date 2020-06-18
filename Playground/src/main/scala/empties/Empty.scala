package empties

object Empty extends Object with IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def union(other: IntSet): IntSet = other
  override def toString: String = "Empty"
}
