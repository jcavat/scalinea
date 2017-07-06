package test.scala.scalinea.grammar

import org.scalatest._
import scalinea.grammar.ToLpInteger._
import scalinea.grammar._


/**
  * Created by joel on 04.07.17.
  */
class LpExpressionTest extends FunSuite {

  val a: LpInteger = "a"
  val b = "b".toLpInteger
  val c = "c".toLpInteger

  test("Default priority") {
    assert( 2 * a + b equals (2*a)+b)
  }

  test("Distributivity *"){

    assert( (a * (2 + b)) isEquals 2*a + a*b )
    assert( (a*(2+b)) isEquals 2*a+a*b )
  }

  test("Trivial rules") {

    assert( a*0 isEquals 0 )
    assert( 0*a isEquals 0 )

    assert( a+0 isEquals a)
    assert( 0+a isEquals a)

    assert( 0 + (a * b) isEquals a * b)
    assert( 1 + (a * 0) isEquals 1 )

  }

  test("Associativity *") {
    assert( (3*b)*c isEquals (3*(b*c)))
  }

  test("Associativity +") {
    assert( a + (2 + b) isEquals (a+2)+b)
  }

  test("Commutativity *") {
    assert( 2 * b isEquals b * 2 )
    assert( a * b isEquals b * a )
  }

  test("Commutativity +") {
    assert( 2 + a isEquals a + 2 )
    assert( b + a isEquals a + b )
  }

  test("Substraction cases") {
    assert( 2 - a isEquals -a + 2 )
    assert( 2 - a - b isEquals 2-(a+b))
    assert( (2 - a) - b isEquals 2-(a+b))
    assert( 2 - a + b isEquals (2-a)+b )
    assert( (2-a)+b isEquals 2-(a-b) )
    assert( (2+a)-b isEquals 2+(a-b) )
  }
}
