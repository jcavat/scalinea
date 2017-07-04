package scalinea

import scalinea.grammar._
import scalinea.grammar.ToLpInteger._

/**
  * Created by joel on 03.07.17.
  */
object Main {
  def main(args: Array[String]): Unit = {


    val a: LpInteger = "a"
    val b = "b".toLpInteger


    println(b)
    println(-200 - 300 - a + 900)
    println(a + 200+ b + 400 + 2*b + 100 + a + 100)
    println(a - 100 + b + 200 - a - 200 + 2*a*b + 100)
    println( 2 * a * (2+a) * b * 5)
    println(200 + a * 2 - 120 + 2 * b + 20 - 40 + 2*b)
    println(100 + 2*(b-100))

    println(b+400 <= a)
    println(b-2 >= a)
    println(b == a)


  }
}
