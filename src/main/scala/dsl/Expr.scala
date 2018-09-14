package ch.hepia.scalinea
package dsl

import util.{MathUtil, Show}

sealed trait Expr {
  def toTerms: clause.Terms = this match {
    case Const(v) if MathUtil.nonZero(v) => clause.Terms.constant( clause.NonZeroConstant(v).get )
    case Const(_) => ???
    case Var(sym) => clause.Terms.singleVar(sym)
    case Add(lhs,rhs) => lhs.toTerms + rhs.toTerms
    case Mult(lhs,rhs) => lhs.toTerms * rhs.toTerms
  }
}
case class Const(value: Double) extends Expr
case class Var(symbol: String) extends Expr
case class Add( lhs: Expr, rhs: Expr ) extends Expr
case class Mult( lhs: Expr, rhs: Expr ) extends Expr

object Ops {

  implicit class RichExpr( lhs: Expr ) {
    def +( rhs: Expr ) = Add( lhs, rhs )
    def *( rhs: Expr ) = Mult( lhs, rhs )
    def -( rhs: Expr ) = Add( lhs, Const(-1)*rhs )
  }

}

object Demo  extends App {

  val x = Var("x")
  val y = Var("y")
  val z = Var("y")
  val one = Const(1)

  import Ops._

  val e = x*(one + x*(one+x))

  println(e)
  
  Show[clause.Terms].print(e.toTerms)

}
