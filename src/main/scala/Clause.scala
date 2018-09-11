package ch.hepia.scalinea

sealed trait Sign 
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

case class Constant(value: Double)

case class NonZeroConstant private(value: Double)
object NonZeroConstant {
  def apply(value: Double): Option[NonZeroConstant] = 
    if(MathUtil.nonZero(value)) 
      Some(new NonZeroConstant(value)) 
    else
      None
}

case class Var(symbol: String)

case class Vars(value: Map[Var, Int])

case class Terms(terms: Map[Vars, Constant])

case class Clause(terms: Terms, sign: Sign, constant: Constant)


object Demo extends App {
  val x = Var("x")
  val y = Var("y")
  val terms = Terms( Map( Vars(Map( x->2, y->1 ))->Constant(2), Vars(Map(x->1))->Constant(5)) )
  val clause = Clause( terms, Sign.Less, Constant(5) )

  println( clause )

}

