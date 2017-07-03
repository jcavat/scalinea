sealed trait LpExpression {
  def +(that: LpExpression) = this match {
    case LpInteger(name) => that
    case LitVal(value) => new AddExpr(value, that)
    case AddExpr(value, expr) => new AddExpr(value, that)
  }
}


case class LpInteger(name: String) extends LpExpression
case class LitVal(value: Int) extends LpExpression
case class AddExpr(value: Int, expr: LpExpression) extends LpExpression

object LpExpression {
  implicit def stringToLpInteger(s: String): LpInteger = LpInteger(s)
  implicit def intToLp(value: Int): LitVal = LitVal(value)


  /*
  trait toLp {
    def toLp(s: String): LpExpression
  }

  implicit object LpInteger extends toLp {
    def toLp(s: String): LpInteger = LpInteger(s)
  }
  */
}

val a: LpInteger = "a"
val b: LpInteger = "b"




//val expr1 = 500*a + 400*b
val expr2 = 400+a;
val expr3: LpExpression = 400;
