package ch.hepia.scalinea

sealed trait Sign {
  import Sign._
  def show: String = this match {
    case Eq => "="
    case NonEq => "≠"
    case BigEq => "≥"
    case LessEq => "≤"
    case Big => ">"
    case Less => "<"
  }
}
object Sign {
  case object Eq extends Sign
  case object NonEq extends Sign
  case object BigEq extends Sign
  case object LessEq extends Sign
  case object Big extends Sign
  case object Less extends Sign
}

object MathUtil {
  val delta: Double = 0.00001
  def nonZero(value: Double): Boolean = math.abs(value) > delta
}

case class Constant(value: Double) {
  def show: String = value.toString
}
case class NonZeroConstant private(value: Double) {
  def show: String = value.toString
}
object NonZeroConstant {
  def apply(value: Double): Option[NonZeroConstant] = 
    if(MathUtil.nonZero(value)) 
      Some(new NonZeroConstant(value)) 
    else
      None
}

case class Exponent private ( value: Int ) {
  def show: String = value.toString
}
object Exponent {
  def apply( value: Int ): Option[Exponent] = 
    if( value != 0 )
      Some( new Exponent(value) )
    else
      None
}

case class Var(symbol: String) {
  def show: String = symbol
}

case class Vars(value: Map[Var, Exponent]) {
  def show: String = {
    value.toList.sortBy( _._1.symbol ).map{
      case (v,ex) if ex.value == 1 => v.show
      case (v,ex) => v.show+"^"+ex.show
    }.mkString("*")
  }
}

case class Terms(terms: Map[Vars, NonZeroConstant]) {
  def show: String = {
    terms.toList.map{
      case (vs,cst) => (cst.show,vs.show)
    }.sortBy( _._2).map{
      case (cst,vs) => cst+"*"+vs
    }.mkString(" + ")
  }
}

case class Clause(terms: Terms, sign: Sign, constant: Constant) {
  def show: String =
     terms.show + " " + sign.show + " " + constant.show
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
  println( clause.show )

}

