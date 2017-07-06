package scalinea.grammar

/**
  * Created by joel on 04.07.17.
  */

abstract class LpConstraint(expr: LpExpression, lit: Cons)

case class GreaterOrEquals(expr: LpExpression, lit: Cons) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " >= " + lit.toString
}
case class Equals(expr: LpExpression, lit: Cons) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " = " + lit.toString
}
case class LessOrEquals(expr: LpExpression, lit: Cons) extends LpConstraint(expr, lit) {
  override def toString: String = expr.toString + " <= " + lit.toString
}

