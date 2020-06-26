package example

object ExampleApp {

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat {
    override def isZero: Boolean = true

    override def predecessor: Nat = throw new IllegalStateException("Zero have no predecessor")

    override def successor: Nat = new Succ(this)

    override def +(that: Nat): Nat = if (!that.isZero) new Succ(this) + that.predecessor else this

    override def -(that: Nat): Nat = if (!that.isZero) throw new IllegalArgumentException("Negative") else this

  }
  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def predecessor: Nat = n

    override def successor: Nat = new Succ(this)

    override def +(that: Nat): Nat = this.successor + that.predecessor

    override def -(that: Nat): Nat = if (!that.isZero) if (!this.isZero) this.predecessor - that.predecessor else throw new IllegalArgumentException("Negative") else this
  }

  def main(args: Array[String]): Unit = {

  }
}





