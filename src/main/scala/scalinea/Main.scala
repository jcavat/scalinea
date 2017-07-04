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



    //val expr1 = 500*a + 400*b
    val expr1: LpExpression = 300
    val expr1bis = 200 - expr1
    val expr2 = 300 + 400 + a + 200 + a
    println(expr2)
  }
}
