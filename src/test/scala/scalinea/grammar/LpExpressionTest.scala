package test.scala.scalinea.grammar

import org.scalatest._
import scalinea.grammar.ToLpInteger._
import scalinea.grammar._


/**
  * Created by joel on 04.07.17.
  */
class LpExpressionTest extends FunSuite {

  test("Expressions simplification") {

    val a: LpInteger = "a"
    val b = "b".toLpInteger

    assert( 2 * a + b equals (2*a)+b)
    println(a*(2+b))
    println(2*a + a*b)
    assert( (a * (2 + b)).simplify() == (2*a + a*b) )

  }



}
