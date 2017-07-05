package scalinea.grammar

/**
  * Created by joel on 04.07.17.
  */

abstract class LpConstraint(expr: LpExpression, lit: LitVal)

case class GreaterOrEquals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " >= " + lit.toString
}
case class Equals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " = " + lit.toString
}
case class LessOrEquals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " <= " + lit.toString
}

