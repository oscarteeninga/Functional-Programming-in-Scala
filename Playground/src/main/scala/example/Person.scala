package example

import scala.collection.mutable.HashMap

class Person(height: Int, weight: Int) {
  var map: HashMap[Int, String] = new HashMap[Int, String]()

  def this() = {
    this(0, 0)
  }

  def min(x: Int, y: Int): Int = if (x < y) x else y
  def max(x: Int, y: Int): Int = if (x < y) y else x
  def addToMap(key: Int, value: String) = map.put(key, value)

  def getFromMap(key: Int):Option[String] = map.get(key)
}
