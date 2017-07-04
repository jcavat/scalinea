package main.scala.scalinea.grammar

import org.scalatest._
import scalinea.grammar._
import scalinea.grammar.ToLpInteger._


/**
  * Created by joel on 04.07.17.
  */
class LpExpressionTest extends FunSuite {

  test("Expressions simplification") {

    val a: LpInteger = "a"
    val b = "b".toLpInteger

    assert( (2 * a + b).toString equals "2 * a + b")
    assert( (a * 2 + b).toString equals "2 * a + b")
    assert( (a * 2 + 2* b).toString equals "2 * a + 2 * b")
    assert( (200 + a * 2 - 120 + 2 * b + 20).toString equals "100 + 2 * a + 2 * b")

    assert( (100 + 2*(a+200)).toString equals "500 + 2 * a")
    assert( (-100 - 2*(a+200)).toString equals "-500 + 2 * a")

  }



}
