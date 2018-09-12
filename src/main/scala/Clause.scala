package ch.hepia.scalinea

sealed trait Sign {
  import Sign._
}

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

object MathUtil {
  val delta: Double = 0.00001
  def nonZero(value: Double): Boolean = math.abs(value) > delta
}

case class Constant(value: Double)
case class NonZeroConstant private(value: Double)
object NonZeroConstant {
  def apply(value: Double): Option[NonZeroConstant] = 
    if(MathUtil.nonZero(value)) 
      Some(new NonZeroConstant(value)) 
    else
      None
}

case class Exponent private ( value: Int )
object Exponent {
  def apply( value: Int ): Option[Exponent] = 
    if( value != 0 )
      Some( new Exponent(value) )
    else
      None
}

case class Var(symbol: String)

case class Vars(value: Map[Var, Exponent]) {
  def sortedKeys = value.keySet.toList.sortBy( _.symbol )
}

object Vars {
  implicit val canShow = Show.instance[Vars]{ vs =>
    vs.sortedKeys.map{ v =>
      val exponent = vs.value(v).value
      if( exponent == 1 )
        v.symbol
      else
        v.symbol+"^"+exponent.toString
    }.mkString("")
  }
}

//TODO: Add canonical ordering for terms
case class Terms(terms: Map[Vars, NonZeroConstant])
object Terms {
  implicit val canShow = Show.instance[Terms]{ ts =>
    ts.terms.toList.map {
      case (vs,NonZeroConstant(x)) =>
        x.toString +"*"+ Show[Vars].asString(vs)
    }.mkString(" + ")
  }
}

case class Clause(terms: Terms, sign: Sign, constant: Constant)
object Clause {
  implicit val canShow = Show.instance[Clause]{
    case Clause(ts,sign,Constant(x)) =>
      Show[Terms].asString(ts) + " " +
     Show[Sign].asString(sign) + " " + x.toString
  }
}

object Demo extends App {
  val x = Var("x")
  val y = Var("y")
  val one = Exponent(1).get
  val two = Exponent(2).get
  val twoC = NonZeroConstant(2).get
  val fiveC = NonZeroConstant(5).get
  val terms =
    Terms(
      Map( Vars(Map( x->two, y->one ))->twoC, Vars(Map(x->one))->fiveC) )
  val clause = Clause( terms, Sign.Less, Constant(5) )

  println( clause )
  Show[Clause].print(clause)

}

