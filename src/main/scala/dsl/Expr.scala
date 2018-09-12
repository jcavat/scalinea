package ch.hepia.scalinea
package dsl

sealed trait Expr {
  def toTerms: clause.Terms = this match {
    case Const(_) => ???
    case Var(sym) => clause.Terms.singleVar(sym)
    case Add(_,_) => ???
  }
}
case class Const(value: Double) extends Expr
case class Var(symbol: String) extends Expr
case class Add( lhs: Expr, rhs: Expr ) extends Expr

object Ops {

  implicit class RichExpr( lhs: Expr ) {
    def +( rhs: Expr ) = Add( lhs, rhs )
  }

}

object Demo  extends App {

  val x = Var("x")
  val y = Var("y")

  import Ops._

  val e = x+y

  println(e)

}
