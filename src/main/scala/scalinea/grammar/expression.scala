package scalinea.grammar

sealed trait LpExpression {
  def +(that: LpExpression): LpExpression = (this, that) match {
    case (l: LitVal, r: LitVal) => LitVal(l.value + r.value)
    case (l: LpExpression, r: LitVal) => AddExpr(r, l)
    case (l: LpExpression, r: PlusUnaryExpr) => AddExpr(l, r.expr)
    case (l: LpExpression, r: MinUnaryExpr) => MinExpr(l, r.expr)
    case _ => AddExpr(this, that)
  }

  def *(that: LpExpression): LpExpression = (this, that) match {
    case (l: LitVal, r: LitVal) => LitVal(l.value * r.value)
    case (l: LpExpression, r: LitVal) => MulExpr(r, l)
    case _ => MulExpr(this, that)
  }

  def -(that: LpExpression): LpExpression = (this, that) match {
    case (l: LitVal, r: LitVal) => LitVal(l.value - r.value)
    case (l: LpExpression, r: LitVal) => MinExpr(LitVal(r.value), l)
    case (l: LpExpression, r: PlusUnaryExpr) => MinExpr(l, r.expr)
    case (l: LpExpression, r: MinUnaryExpr) => AddExpr(l, r.expr)
    case _ => MinExpr(this, that)
  }

  def unary_-(): LpExpression = this match {
    case LitVal(v) => LitVal(-v)
    case AddExpr(l, r) => MinExpr(-l, r)
    case _ => MinUnaryExpr(this)
  }

  def unary_+(): LpExpression = this match {
    case _ => PlusUnaryExpr(this)
  }

  override def toString: String = {
    this match {
      case LpInteger(name) => name
      case LitVal(v) => v.toString
      case AddExpr(l,r) => l.toString + " + " + r.toString
      case MulExpr(l,r) => l.toString + " * " + r.toString
      case MinExpr(l,r) => l.toString + " - " + r.toString
      case MinUnaryExpr(e) => "-" + e.toString
      case PlusUnaryExpr(e) => "+" + e.toString
    }
  }
}

case class LpInteger(name: String) extends LpExpression

case class LitVal(value: Int) extends LpExpression

case class AddExpr(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class MulExpr(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class MinExpr(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class MinUnaryExpr(expr: LpExpression) extends LpExpression

case class PlusUnaryExpr(expr: LpExpression) extends LpExpression

trait ToLpInteger[A] {
  def toLpInteger(a: A): LpInteger
}

object ToLpInteger {

  implicit object StringToLpInteger extends ToLpInteger[String] {
    def toLpInteger(a: String): LpInteger = LpInteger(a)
  }

  implicit class ToLpUtil[A](x: A) {
    def toLpInteger(implicit subject: ToLpInteger[A]) = {
      subject.toLpInteger(x)
    }
  }

}

object LpExpression {
  implicit def stringToLpInteger(s: String): LpInteger = LpInteger(s)

  implicit def intToLpInteger(s: Int): LitVal = LitVal(s)
}
