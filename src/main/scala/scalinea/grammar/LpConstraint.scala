package scalinea.grammar

/**
  * Created by joel on 04.07.17.
  */

abstract class LpConstraint(expr: LpExpression, lit: LitVal)

case class GreaterOrEquals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit)
case class Equals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit)
case class LessOrEquals(expr: LpExpression, lit: LitVal) extends LpConstraint(expr, lit)

