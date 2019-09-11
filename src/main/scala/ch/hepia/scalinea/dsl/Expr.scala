package ch.hepia.scalinea
package dsl

import ch.hepia.scalinea.dsl.System.Maximize
import ch.hepia.scalinea.format.LPFormat
import util.{MathUtil, Show}

sealed trait Expr {
  def toTerms: clause.Terms = this match {
    case Const(v) if MathUtil.nonZero(v) => clause.Terms.constant( clause.NonZeroConstant(v).get )
    case Const(_) => clause.Terms.empty
    case Var(sym, minBound, maxBound) => clause.Terms.singleContinuousVar(sym, minBound, maxBound)
    case IVar(sym, minBound, maxBound) => clause.Terms.singleIntegerVar(sym, minBound, maxBound)
    case BVar(sym) => clause.Terms.singleBinaryVar(sym)
    case Add(lhs,rhs) => lhs.toTerms + rhs.toTerms
    case Mult(lhs,rhs) => lhs.toTerms * rhs.toTerms
  }
  def isZero: Boolean = false
}
case class Const(value: Double) extends Expr {
  override def isZero: Boolean = MathUtil.isZero(value)
}

sealed trait NamedVar extends Expr {
  def symbol: String
}
sealed trait NumVar extends NamedVar

// Default Continuous var
case class Var(symbol: String, minBound: Option[Double] = None, maxBound: Option[Double] = None) extends NumVar {
  def free: Var = copy( minBound = Some(Double.NegativeInfinity), maxBound = Some(Double.PositiveInfinity) )
  def minBound(min: Double): Var = copy( minBound=Some(min) )
  def maxBound(max: Double): Var = copy( maxBound=Some(max) )
  def range(min: Double, max: Double): Var = minBound(min).maxBound(max)
}

// Integer/generals var
case class IVar(symbol: String, minBound: Option[Int] = None, maxBound: Option[Int] = None) extends NumVar {
  def minBound(min: Int): IVar = copy( minBound=Some(min) )
  def maxBound(max: Int): IVar = copy( maxBound=Some(max) )
  def range(min: Int, max: Int): IVar = minBound(min).maxBound(max)
}

// Boolean var
case class BVar(symbol: String) extends NamedVar { }

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
    def unary_- = Mult(Const(-1), lhs )
    def <=( rhs: Expr ) = LessEq( lhs, rhs )
    def <=( rhs: Double ) = LessEq( lhs, Const(rhs) )
    //def <( rhs: Expr ) = Less( lhs, rhs )
    //def <( rhs: Double) = Less( lhs, Const(rhs) )
    def >=( rhs: Expr ) = BigEq( lhs, rhs )
    def >=( rhs: Double ) = BigEq( lhs, Const(rhs) )
    //def >( rhs: Expr ) = Big( lhs, rhs )
    //def >( rhs: Double ) = Big( lhs, Const(rhs) )
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

  def sum( expr: Expr, exprs: Expr* ): Expr = exprs.foldLeft(expr)( _ + _ )
  def sum( exprs: Iterable[Expr] ): Expr = exprs.reduceLeft( _ + _ )
}


