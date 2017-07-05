package scalinea.grammar

sealed trait LpExpression {
  def +(that: LpExpression): LpExpression = AddExpr(this, that).simplify()

  def *(that: LpExpression): LpExpression = MulExpr(this, that).simplify()

  def -(that: LpExpression): LpExpression = MinExpr(this, that).simplify()

  def <=(that: LpExpression) : LpConstraint = {
    val (LitVal(v), expr) = MinExpr(this, that).simplify().extractConstant()
    LessOrEquals(expr, LitVal(-v))
  }
  def >=(that: LpExpression) : LpConstraint = {
    val (LitVal(v), expr) = MinExpr(this, that).simplify().extractConstant()
    GreaterOrEquals(expr, LitVal(-v))
  }
  def ==(that: LpExpression) : LpConstraint = {
    val (LitVal(v), expr) = MinExpr(this, that).simplify().extractConstant()
    Equals(expr, LitVal(-v))
  }

  def unary_-(): LpExpression = MinUnaryExpr(this).simplify()

  def unary_+(): LpExpression = this.simplify()

  def extractConstant(): (LitVal, LpExpression) = this match {
    case AddExpr(lit: LitVal, expr) => (lit, expr)
    case MinExpr(lit: LitVal, expr) => (lit, MinUnaryExpr(expr).simplify())
    case expr => (LitVal(0), expr)
  }

  def simplify(): LpExpression = this match {

    case lpi: LpInteger => lpi
    case lit: LitVal => lit

    case MulExpr(LitVal(l1), LitVal(l2)) => LitVal(l1*l2).simplify()

    // +- rules
    case AddExpr(l, MinUnaryExpr(r)) => MinExpr(l, r).simplify()
    case MinExpr(l, MinUnaryExpr(r)) => AddExpr(l, r).simplify()
    case MinUnaryExpr(MinUnaryExpr(l)) => l.simplify()

    // trivial rules
    case MinUnaryExpr(LitVal(v)) => LitVal(-v).simplify()
    case AddExpr(LitVal(0), expr) => expr
    case MulExpr(LitVal(0), expr) => LitVal(0)
    case MinExpr(expr, LitVal(0)) => expr

    // Place literal first
    case AddExpr(expr, rlit @ LitVal(r)) => AddExpr(rlit, expr).simplify()
    case MinExpr(expr, rlit @ LitVal(r)) => AddExpr(LitVal(-r), expr).simplify()
    case MulExpr(expr, rlit @ LitVal(r)) => MulExpr(rlit, expr).simplify()

    // Associativity +/-
    case AddExpr(LitVal(l1), AddExpr(LitVal(l2), expr)) => AddExpr(LitVal(l1 + l2), expr).simplify()
    case AddExpr(LitVal(l1), MinExpr(LitVal(l2), expr)) => MinExpr(LitVal(l1 + l2), expr).simplify()
    case MinExpr(LitVal(l1), MinExpr(LitVal(l2), expr)) => MinExpr(LitVal(l1 - l2), expr).simplify()
    case MinExpr(LitVal(l1), AddExpr(LitVal(l2), expr)) => AddExpr(LitVal(l1 - l2), expr).simplify()

    // Associativity *
    case MulExpr(LitVal(l1), MulExpr(LitVal(l2), expr)) => MulExpr(LitVal(l1 * l2), expr).simplify()

    // (a+b) + c = a + (b+c)
    case AddExpr(AddExpr(a,b), c) => AddExpr(a, AddExpr(b, c)).simplify()

    // (a+b) - c = a + (b-c)
    case MinExpr(AddExpr(a,b), c) => AddExpr(a, MinExpr(b, c)).simplify()

    // (a*b)*c = a*(b*c)
    case MulExpr(MulExpr(a,b),c) => MulExpr(a, MulExpr(b,c)).simplify()

    // i*(a+b) = i*a+i*b
    case MulExpr(lit @ LitVal(i), AddExpr(a,b)) => AddExpr(MulExpr(lit, a), MulExpr(lit, b)).simplify()

    case MulExpr(a,b) => MulExpr(a.simplify(), b.simplify())
    case AddExpr(a,b) => AddExpr(a.simplify(), b.simplify())
    case MinExpr(a,b) => MinExpr(a.simplify(), b.simplify())

    case _ => this

  }

  override def toString: String = {
    this match {
      case LpInteger(name) => name
      case LitVal(v) => v.toString
      case AddExpr(l,r) => "("+ l.toString + " + " + r.toString +")"
      case MulExpr(l,r) =>  "("+l.toString + " * " + r.toString +")"
      case MinExpr(l,r) =>  "("+l.toString + " - " + r.toString +")"
      case MinUnaryExpr(e) => e.toString
      case s => s.toString
    }
/*
this match {
  case LpInteger(name) => name
  case LitVal(v) => v.toString
  case AddExpr(l,r) => l.toString + " + " + r.toString
  case MulExpr(l,r) => l.toString + " * " + r.toString
  case MinExpr(l,r) => l.toString + " - " + r.toString
  case MinUnaryExpr(e) => e.toString
  case s => s.toString
}
    */
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

