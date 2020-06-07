package example

object Main extends App {

    def factorial(x: Long): Long = {
        factorialTail(x, x.toInt)
    }
    def factorialTail(x: Long, i: Int): Long = {
        if (i == 0) x else factorialTail(x*i, i-1)
    }

    println(factorial(10))
}
