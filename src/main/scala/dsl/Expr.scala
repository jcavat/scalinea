package ch.hepia.scalinea
package dsl

import util.{LpFormat, MathUtil, Show}

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

sealed trait Constr {
  import Ops._

  private def makeClause( lhs: Expr, rhs: Expr, sign: clause.Sign ) =
    clause.Clause( (lhs-rhs).toTerms, sign )

  def toClause: clause.Clause = this match {
    case LessEq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.LessEq )
    case Less(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Less )
    case BigEq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.BigEq )
    case Big(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Big )
    case Eq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Eq )
  }
}

case class LessEq(lhs: Expr, rhs: Expr) extends Constr
case class Less(lhs: Expr, rhs: Expr) extends Constr
case class BigEq(lhs: Expr, rhs: Expr) extends Constr
case class Big(lhs: Expr, rhs: Expr) extends Constr
case class Eq(lhs: Expr, rhs: Expr) extends Constr



object Ops {

  implicit class RichExpr( lhs: Expr ) {
    def +( rhs: Expr ) = Add( lhs, rhs )
    def +( rhs: Double ) = Add( lhs, Const(rhs) )
    def *( rhs: Expr ) = Mult( lhs, rhs )
    def *( rhs: Double ) = Mult( lhs, Const(rhs) )
    def -( rhs: Expr ) = Add( lhs, Const(-1)*rhs )
    def -( rhs: Double ) = Add( lhs, Const(-1)*rhs )
    def <=( rhs: Expr ) = LessEq( lhs, rhs )
    def <=( rhs: Double ) = LessEq( lhs, Const(rhs) )
    def <( rhs: Expr ) = Less( lhs, rhs )
    def <( rhs: Double) = Less( lhs, Const(rhs) )
    def >=( rhs: Expr ) = BigEq( lhs, rhs )
    def >=( rhs: Double ) = BigEq( lhs, Const(rhs) )
    def >( rhs: Expr ) = Big( lhs, rhs )
    def >( rhs: Double ) = Big( lhs, Const(rhs) )
    def ===( rhs: Expr ) = Eq( lhs, rhs )
    def ===( rhs: Double) = Eq( lhs, Const(rhs) )
  }

  implicit class RichDouble( val lhs: Double ) extends RichLiteral

  implicit class RichInt( val lhsArg: Int ) extends RichLiteral {
    val lhs: Double = lhsArg.toDouble
  }

  sealed trait RichLiteral {
    def lhs: Double
    def +( rhs: Expr ) = Add( Const(lhs), rhs )
    def *( rhs: Expr ) = Mult( Const(lhs), rhs )
    def -( rhs: Expr ) = Add( Const(lhs), Const(-1)*rhs )
    def <=( rhs: Expr ) = LessEq( Const(lhs), rhs )
    def <( rhs: Expr ) = Less( Const(lhs), rhs )
    def >=( rhs: Expr ) = BigEq( Const(lhs), rhs )
    def >( rhs: Expr ) = Big( Const(lhs), rhs )
    def ===( rhs: Expr ) = Eq( Const(lhs), rhs )
  }

}

object Demo  extends App {

  val x = Var("x")
  val y = Var("y")
  val z = Var("z")
  val one = Const(1)

  import Ops._

  //val e = 2*x*x + 20 === 4*x - 3 + 3*x*y + 3*y + 18
  val e = 3*x + 2*y + 5*z + 5 >= 3*one

  println(e)
  
  Show[clause.Clause].print(e.toClause)
  LpFormat[clause.Clause].print(e.toClause)

}
