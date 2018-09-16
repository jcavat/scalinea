package ch.hepia.scalinea
package clause

import util.MathUtil.nonZero
import util.{LpFormat, Show}

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

  implicit val canExportToLp = LpFormat.instance[Sign]{
    case Eq => "="
    case NonEq => "!="
    case BigEq => ">="
    case LessEq => "<="
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

case class Var(symbol: String)

case class Vars(value: Map[Var, Exponent]) {
  import Vars._

  def sortedVars = value.keySet.toList.sortBy( _.symbol )

  def isLinear = this == constant || ( value.size == 1 && value.head._2 == Exponent.one )

  def *(that: Vars): Vars = {
    val varMap = (this.value.keySet ++ that.value.keySet).map { v =>
      (this.value.get(v), that.value.get(v)) match {
        case (Some(e1), Some(e2)) => v -> (e1 + e2)
        case (None, Some(c)) => v -> Some(c)
        case (Some(c), None) => v -> Some(c)
        case (None, None) => v -> None // Unreachable code
      }
    }.filter( _._2.isDefined ).map { case (v, o) => (v, o.get) }.toMap

    Vars(varMap)
  }
}

object Vars {

  val constant: Vars = Vars( Map() )

  def singleVar( symb: String ): Vars =
    Vars( Map( Var(symb) -> Exponent.one ) ) 

  implicit val canShow = Show.instance[Vars]{ vs =>
    vs.sortedVars.map{ v =>
      val exponent = vs.value(v).value
      if( exponent == 1 )
        v.symbol
      else
        v.symbol+"^"+exponent.toString
    }.mkString("*")
  }

  implicit val canExportToLp = LpFormat.instance[Vars]{ vs =>
    vs.sortedVars.map{ v =>
      val exponent = vs.value(v).value
      if( exponent == 1 )
        v.symbol
      else
        v.symbol+" ^ "+exponent.toString
    }.mkString(" ")
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
        case ((v1@Var(h1))::t1,(v2@Var(h2))::t2) => {
          if(h1 < h2) -1
          else if( h1 > h2 ) 1
          else if( lhs.value(v1).value < rhs.value(v2).value ) -1
          else if( lhs.value(v1).value > rhs.value(v2).value ) 1
          else cmp(t1,t2)
        }
      }
      cmp(lhs.sortedVars,rhs.sortedVars)
    }
  }
}

case class Terms(terms: Map[Vars, NonZeroConstant]) {
  def sortedVars = terms.keySet.toList.sorted

  def +(that: Terms): Terms = {
    val termMap = (this.terms.keySet ++ that.terms.keySet).map { vars =>
      (this.terms.get(vars), that.terms.get(vars)) match {
        case (Some(c1), Some(c2)) => vars -> (c1 + c2)
        case (None, Some(c)) => vars -> Some(c)
        case (Some(c), None) => vars -> Some(c)
        case (None, None) => vars -> None // Unreachable code
      }
    }.filter( _._2.isDefined ).map { case (vars, o) => (vars, o.get) }.toMap

    Terms(termMap)
  }

  def *(that: Terms): Terms = {
    val tuples = for {
      lhs <- this.terms.toList
      rhs <- that.terms.toList
    } yield (Terms.mulTerm(lhs, rhs))

    tuples.map{
      case (vs, nzc) => Terms( Map(vs -> nzc) )
    }.reduceLeft(_+_)

  }



}
object Terms {

  def constant(value: NonZeroConstant): Terms = Terms( Map(Vars.constant -> value) )

  def singleVar( symb: String ): Terms = {
    Terms( Map( Vars.singleVar(symb) -> NonZeroConstant.one ) )
  }

  implicit val canShow = Show.instance[Terms]{ ts =>
    ts.sortedVars.map {
      case vs =>
        val const = ts.terms(vs).value
        const.toString +"*"+ Show[Vars].asString(vs)
    }.mkString(" + ")
  }

  implicit val canExportToLp = LpFormat.instance[Terms]{ ts =>

    val (linearVars, quadVars) = ts.sortedVars.partition( vars => vars.isLinear )

    val toStr: List[Vars] => List[String] =
      _.map( vars => ts.terms(vars).value.toString + " " + LpFormat[Vars].asString(vars) )

    val linear = toStr(linearVars).mkString(" + ")
    val quad = toStr(quadVars).mkString(" + ")

    linear + ( if (quad.isEmpty) "" else " + [ " + quad + " ]" )

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

  implicit val canExportToLp = LpFormat.instance[Clause]{
    case Clause(ts,sign) => {

      ts.terms.get( Vars(Map()) ) match {
        case Some( v ) =>
          LpFormat[Terms].asString(Terms(ts.terms - Vars(Map()))) + " " +
            LpFormat[Sign].asString(sign) + " " + -v.value

        case None =>
          LpFormat[Terms].asString(ts) + " " +
            LpFormat[Sign].asString(sign) + " 0"
      }
    }
  }
}

object Demo {
  val x = Var("x")
  val y = Var("y")
  val one = Exponent(1).get
  val two = Exponent(2).get
  val twoC = NonZeroConstant(2).get
  val fiveC = NonZeroConstant(5).get
  val terms =
    Terms(
      Map( Vars(Map( x->two, y->one ))->twoC, Vars(Map(x->one))->fiveC) )
  val clause = Clause( terms, Sign.BigEq )

  println( clause )
  Show[Clause].print(clause)

}

