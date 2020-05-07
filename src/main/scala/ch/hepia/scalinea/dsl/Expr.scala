package ch.hepia.scalinea
package dsl

import ch.hepia.scalinea.dsl.System.Maximize
import ch.hepia.scalinea.format.LPFormat
import util.{MathUtil, Show}

sealed trait Expr {
  def toTerms: clause.Terms = this match {
    case Const(v) if MathUtil.nonZero(v) =>
      clause.Terms.constant( clause.NonZeroConstant(v).get )
    case Const(_) => clause.Terms.empty
    case Var(sym, minBound, maxBound) =>
      clause.Terms.singleContinuousVar(sym, minBound, maxBound)
    case IVar(sym, minBound, maxBound) =>
      clause.Terms.singleIntegerVar(sym, minBound, maxBound)
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
  def range( r: Range): IVar = range( r.head, r.last )
}

case class Add( lhs: Expr, rhs: Expr ) extends Expr
case class Mult( lhs: Expr, rhs: Expr ) extends Expr


sealed trait BExpr extends Constr {
  import BExpr.CNF
  private def merge( lhs: CNF, rhs: CNF ): CNF = {
    val merger: (Sign,Sign) => Option[Sign] = {
      case (Sign.Negate, Sign.Negate) => Some(Sign.Negate)
      case (Sign.Positive, Sign.Positive) => Some(Sign.Positive)
      case _ => Some(Sign.Tautology)
    }
    val vars: Map[BVar, Sign] = util.MapUtil.mergeOpt(lhs.vars, rhs.vars, merger)
    CNF( vars )
  }
  def normalize: List[CNF] = {
    this match {
      case b:BVar => List( CNF.single(b, sign = Sign.Positive) )
      case Not(b:BVar) => List( CNF.single(b, sign = Sign.Negate) )
      case Not(Not(bexpr)) => bexpr.normalize
      case Not( Or(lhs,rhs) ) => And(Not(lhs),Not(rhs)).normalize
      case Not( And(lhs,rhs) ) => Or(Not(lhs),Not(rhs)).normalize
      case Not( Implies(lhs,rhs) ) => And( lhs, Not(rhs) ).normalize
      case Not( Iff(lhs,rhs) ) => Or(
        And(lhs,Not(rhs)),
        And(rhs,Not(lhs))
      ).normalize
      case Implies(lhs,rhs) => Or(Not(lhs),rhs).normalize
      case Iff(lhs,rhs) => And(Implies(lhs,rhs),Implies(rhs,lhs)).normalize
      case And( lhs, rhs ) => lhs.normalize ++ rhs.normalize //TODO: Check not satisfiable
      case Or( And(x,y), z ) => And( Or(x,z), Or(y,z) ).normalize
      case Or( x, And(y,z) ) => And( Or(x,y), Or(x,z) ).normalize
      case Or( lhs, rhs ) => {
        val lhsN: List[CNF] = lhs.normalize
        val rhsN: List[CNF] = rhs.normalize
        for {
          l <- lhsN
          r <- rhsN
        } yield merge(l,r)
      }
    }
  }

  private def toNumeric: List[Constr] = {
    import Ops._
    normalize.map { cnf =>
      val vars: Map[BVar, Sign] = cnf.vars
      val lhs: Expr = vars.map {
        case (v,Sign.Positive) => v
        case (v,Sign.Negate) => 1-v
        case (v,_) => 1-v + v //TODO: (v-v)
      }.reduce( _ + _ )

      lhs >= 1
    }
  }

  override def toClause: List[clause.Clause] = {
    toNumeric.flatMap( _.toClause )
  }
}

sealed trait Sign
object Sign {
  case object Positive extends Sign
  case object Negate extends Sign
  case object Tautology extends Sign
}

object BExpr {
  case class CNF( vars: Map[BVar,Sign] )
  object CNF {
    def single( bvar: BVar, sign: Sign ) = CNF( Map(bvar->sign) )
  }
}

// Boolean var
case class BVar(symbol: String) extends NamedVar with BExpr
case class Or( lhs: BExpr, rhs: BExpr ) extends BExpr
case class And( lhs: BExpr, rhs: BExpr ) extends BExpr
case class Not( expr: BExpr ) extends BExpr
case class Implies( lhs: BExpr, rhs: BExpr ) extends BExpr
case class Iff( lhs: BExpr, rhs: BExpr ) extends BExpr


sealed trait Constr {
  import Ops._

  private def makeClause( lhs: Expr, rhs: Expr, sign: clause.Sign ) =
    List( clause.Clause( (lhs-rhs).toTerms, sign ) )

  def toClause: List[clause.Clause] = this match {
    case LessEq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.LessEq )
    case Less(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Less )
    case BigEq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.BigEq )
    case Big(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Big )
    case Eq(lhs, rhs) => makeClause( lhs, rhs, clause.Sign.Eq )
    case be: BExpr => be.toClause
  }
}

case class LessEq(lhs: Expr, rhs: Expr) extends Constr
case class Less(lhs: Expr, rhs: Expr) extends Constr
case class BigEq(lhs: Expr, rhs: Expr) extends Constr
case class Big(lhs: Expr, rhs: Expr) extends Constr
case class Eq(lhs: Expr, rhs: Expr) extends Constr



object Ops {

  implicit class RichBooleanExpr( lhs: BExpr ) {
    def `imply`(rhs: BExpr) = Implies(lhs, rhs)
    def `iif`(rhs: BExpr) = Iff(lhs, rhs)
    def &(rhs: BExpr) = And(lhs, rhs)
    def |(rhs: BExpr) = Or(lhs, rhs)
    def unary_! = Not(lhs)
  }
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

  // TODO: Add first elem in a list
  def sum( exprs: Iterable[Expr] ): Expr = exprs.reduceLeft( _ + _ )
  def sum[T]( exprs: Iterable[T] )(f: T => Expr): Expr = exprs.map(f).reduceLeft( _ + _ )
  def forAll[T](vs: Iterable[T])(f: T => dsl.Constr): Iterable[dsl.Constr] = vs.map(f)
  def or( exprs: Iterable[BExpr] ): BExpr = exprs.reduceLeft( _ | _ )
  def or( exprs: BExpr* ): BExpr = exprs.reduceLeft( _ | _ )
  def and( exprs: Iterable[BExpr] ): BExpr = exprs.reduceLeft( _ & _ )
  def and( exprs: BExpr* ): BExpr = exprs.reduceLeft( _ & _ )

  def exactlyOneOf( exprs: BExpr* ): BExpr = {
    val e: Seq[BExpr] = exprs.map(bexpr => bexpr & !or(exprs.filter(_ != bexpr)))
    or(e)
  }

  /*
    a only b only c === a and not(or(b,c)) OR b and not(or(a,c)) OR ...

1 0 0
0 1 0
0 0 1
   */
}


