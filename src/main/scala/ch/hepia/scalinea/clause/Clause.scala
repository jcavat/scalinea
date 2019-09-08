package ch.hepia.scalinea
package clause

import util.MathUtil.nonZero
import util.{Show, MapUtil}

sealed trait Sign 

object Sign {
  case object Eq extends Sign
  case object NonEq extends Sign
  case object BigEq extends Sign
  case object LessEq extends Sign
  case object Big extends Sign
  case object Less extends Sign

  implicit val canShow = Show.instance[Sign]{
    case Eq => "="
    case NonEq => "≠"
    case BigEq => "≥"
    case LessEq => "≤"
    case Big => ">"
    case Less => "<"
  }

}


case class NonZeroConstant private(value: Double) {
  def +(that: NonZeroConstant): Option[NonZeroConstant] = NonZeroConstant(this.value+that.value)
  def *(that: NonZeroConstant): NonZeroConstant = new NonZeroConstant(value*that.value)
}
object NonZeroConstant {
  def apply(value: Double): Option[NonZeroConstant] = 
    if(nonZero(value)) 
      Some(new NonZeroConstant(value)) 
    else
      None
  val one = new NonZeroConstant(1.0)
}

case class Exponent private ( value: Int ) {
  def +(that: Exponent): Option[Exponent] = Exponent(value + that.value)
}
object Exponent {
  def apply( value: Int ): Option[Exponent] = 
    if( value != 0 )
      Some( new Exponent(value) )
    else
      None

  val one = new Exponent(1)
}

sealed trait Var {
  type Value
  def symbol: String
}
sealed trait NumVar extends Var {
  def minBound: Option[Value]
  def maxBound: Option[Value]
  def isBounded: Boolean = minBound.isDefined || maxBound.isDefined
}
case class BinaryVar(symbol: String ) extends Var {
  type Value = Boolean
}
case class IntegerVar(symbol: String, minBound: Option[Int], maxBound: Option[Int] ) extends NumVar {
  type Value = Int
}
case class ContinuousVar(symbol: String, minBound: Option[Double] = None, maxBound: Option[Double] = None) extends NumVar {
  type Value = Double
  def isMinBoundInfinity : Boolean = minBound.contains( Double.NegativeInfinity )
  def isMaxBoundInfinity : Boolean = maxBound.contains( Double.PositiveInfinity )
  def isFree: Boolean = isMinBoundInfinity && isMaxBoundInfinity
  override def isBounded: Boolean = !isFree && super.isBounded
}

case class Vars(value: Map[Var, Exponent]) {
  import Vars._

  def sortedVar = value.keySet.toList.sortBy( _.symbol )

  def isLinear = this == constant || ( value.size == 1 && value.head._2 == Exponent.one )

  def *(that: Vars): Vars = {
    val varMap = MapUtil.mergeOpt[Var,Exponent]( this.value, that.value, _ + _ )
    Vars(varMap)
  }
}

object Vars {

  val constant: Vars = Vars( Map() )

  def singleVar( v: Var ): Vars =
    Vars( Map( v -> Exponent.one ) )

  implicit val canShow = Show.instance[Vars]{ vs =>
    vs.sortedVar.map{ v =>
      val exponent = vs.value(v).value
      if( exponent == 1 )
        v.symbol
      else
        v.symbol+"^"+exponent.toString
    }.mkString("*")
  }

  /* Canonical ordering:
   * - alphabetic per first symbol
   * - less vars first
   * - small exponent first
   */
  implicit val canOrder = new Ordering[Vars] {
    def compare( lhs: Vars, rhs: Vars ): Int = {
      def cmp( vs1: List[Var], vs2: List[Var] ): Int = (vs1,vs2) match {
        case (Nil,Nil) => 0
        case (Nil,_) => 1
        case (_,Nil) => -1
        case ( v1::t1, v2::t2) => {
          val h1 = v1.symbol
          val h2 = v2.symbol
          if(h1 < h2) -1
          else if( h1 > h2 ) 1
          else if( lhs.value(v1).value < rhs.value(v2).value ) -1
          else if( lhs.value(v1).value > rhs.value(v2).value ) 1
          else cmp(t1,t2)
        }
      }
      cmp(lhs.sortedVar,rhs.sortedVar)
    }
  }
}

case class Terms(terms: Map[Vars, NonZeroConstant]) {
  def sortedVars = terms.keySet.toList.sorted

  def +(that: Terms): Terms = {
    val termMap = MapUtil.mergeOpt[Vars,NonZeroConstant](this.terms, that.terms, _+_ )
    Terms(termMap)
  }

  def *(that: Terms): Terms = {
    val tuples = for {
      lhs <- this.terms.toList
      rhs <- that.terms.toList
    } yield Terms.mulTerm(lhs, rhs)

    if (tuples.isEmpty)
      return Terms.empty

    tuples.map{
      case (vs, nzc) => Terms( Map(vs -> nzc) )
    }.reduceLeft(_+_)
  }
}
object Terms {

  def constant(value: NonZeroConstant): Terms = Terms( Map(Vars.constant -> value) )

  def empty: Terms = Terms( Map() )

  def singleContinuousVar(symbol: String, minBound: Option[Double] = None, maxBound: Option[Double] ): Terms = {
    Terms( Map( Vars.singleVar(ContinuousVar(symbol, minBound, maxBound)) -> NonZeroConstant.one ) )
  }
  def singleIntegerVar(symbol: String, minBound: Option[Int] = None, maxBound: Option[Int] ): Terms = {
    Terms( Map( Vars.singleVar(IntegerVar(symbol, minBound, maxBound)) -> NonZeroConstant.one ) )
  }
  def singleBinaryVar(symbol: String): Terms = {
    Terms( Map( Vars.singleVar(BinaryVar(symbol)) -> NonZeroConstant.one ) )
  }

  implicit val canShow = Show.instance[Terms]{ ts =>
    ts.sortedVars.map (
      vs => {
        val const = ts.terms(vs).value
        const.toString +"*"+ Show[Vars].asString(vs)
      }
    ).mkString(" + ")
  }

  private def mulTerm(lhs: (Vars, NonZeroConstant), rhs: (Vars, NonZeroConstant)): (Vars, NonZeroConstant) = {
    (lhs._1 * rhs._1, lhs._2 * rhs._2)
  }

}

case class Clause(terms: Terms, sign: Sign)
object Clause {
  implicit val canShow = Show.instance[Clause]{
    case Clause(ts,sign) =>
      Show[Terms].asString(ts) + " " +
     Show[Sign].asString(sign) + " 0"
  }
}

