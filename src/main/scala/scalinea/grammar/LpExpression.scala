package scalinea.grammar

sealed trait LpExpression {
  def +(that: LpExpression): LpExpression = Add(this, that).simplify()

  def *(that: LpExpression): LpExpression = Mul(this, that).simplify()

  def -(that: LpExpression): LpExpression = Sub(this, that).simplify()

  def <=(that: LpExpression) : LpConstraint = {
    val (Cons(v), expr) = Sub(this, that).simplify().extractConstant()
    LessOrEquals(expr, Cons(-v))
  }
  def >=(that: LpExpression) : LpConstraint = {
    val (Cons(v), expr) = Sub(this, that).simplify().extractConstant()
    GreaterOrEquals(expr, Cons(-v))
  }
  def ==(that: LpExpression) : LpConstraint = {
    val (Cons(v), expr) = Sub(this, that).simplify().extractConstant()
    Equals(expr, Cons(-v))
  }

  def unary_-(): LpExpression = Minus(this).simplify()

  def unary_+(): LpExpression = this.simplify()

  def extractConstant(): (Cons, LpExpression) = this match {
    case Add(expr, lit: Cons) => (lit, expr)
    case Sub(expr, Cons(v)) => (Cons(-v), expr.simplify())
    case expr => (Cons(0), expr)
  }

 def simplify(): LpExpression = {
   println("- " + this);
   this match {

     case lpi: LpInteger => lpi
     case lit: Cons => lit

     // Simplification
     case Mul(Cons(l1), Cons(l2)) => Cons(l1 * l2)
     case Add(Cons(l1), Cons(l2)) => Cons(l1 + l2)
     case Sub(Cons(l1), Cons(l2)) => Cons(l1 - l2)

     // trivial rules
     case Minus(Cons(v)) => Cons(-v).simplify()
     case Add(expr, Cons(0)) => expr.simplify()
     case Mul(Cons(0), expr) => Cons(0)

     // +- UNARY RULES
     case Sub(l, Minus(r)) => Add(l, r).simplify()
     case Add(l, Minus(r)) => Sub(l, r).simplify()
     case Add(l, Cons(r)) if r < 0 => Sub(l, Cons(-r)).simplify()
     case Minus(Minus(l)) => l.simplify()

     case Minus(Add(a, b)) => Sub(Minus(a), b).simplify() ////////// BBBBBBBBBBBBBBBBBBBB

     // Place literal first for *
     case Mul(expr, rlit@Cons(r)) => Mul(rlit, expr).simplify()
     // (a+b)*c = c*(a+b)
     case Mul(Add(a, b), c) => Mul(c, Add(a, b)).simplify()

     // DISTRIBUTIVITY
     // i*(a+b) = i*a+i*b
     case Mul(expr, Add(a, b)) => Add(Mul(expr, a), Mul(expr, b)).simplify()
     // i*(a-b) = i*a-i*b
     case Mul(expr, Sub(a, b)) => Sub(Mul(expr, a), Mul(expr, b)).simplify()

     // Place literal at the end for +/-
     case Add(c: Cons, expr) => Add(expr, c).simplify()
     // c-expr = -expr+c
     //   case Sub(c: Cons, expr) => Add(Minus(expr), c).simplify() ///// AAAAAAAAAAAAAAAAAAAAAAAAAAA

     // Simplification consts +/-
     // (expr+c1)+c2 = expr+(c1+c2)
     case Add(Add(expr, Cons(l1)), Cons(l2)) => Add(expr, Cons(l1 + l2)).simplify()
     // (expr-c1)+c2 = expr+(c2-c1)
     case Add(Sub(expr, Cons(l1)), Cons(l2)) => Add(expr, Cons(l2 - l1)).simplify()
     // (expr-c1)-c2 = expr-(c1+c2)
     case Sub(Sub(expr, Cons(l1)), Cons(l2)) => Sub(expr, Cons(l1 + l2)).simplify()
     // (expr+c1)-c2 = expr+(c1-c2)
     case Sub(Add(expr, Cons(l1)), Cons(l2)) => Add(expr, Cons(l1 - l2)).simplify()
     // (c1-expr)-c2 = -expr+(c1-c2)
     case Sub(Sub(Cons(c1), expr), Cons(c2)) => Add(Minus(expr), Cons(c1 - c2)).simplify()
     // (c1-expr)+c2 = -expr+(c1+c2)
     case Add(Sub(Cons(c1), expr), Cons(c2)) => Add(Minus(expr), Cons(c1 + c2)).simplify()

     // Extract the const
     // (expr1+c)+expr2 = (expr1+expr2)+c
     case Add(Add(expr1, c: Cons), expr2) => Add(Add(expr1, expr2), c).simplify()
     // (expr1-c)+expr2 = (expr1+expr2)-c
     case Add(Sub(expr1, c: Cons), expr2) => Sub(Add(expr1, expr2), c).simplify()
     // (expr1+c)-expr2 = (expr1-expr2)+c
     case Sub(Add(expr1, c: Cons), expr2) => Add(Sub(expr1, expr2), c).simplify()
     // (expr1-c)-expr2 = (expr1-expr2)-c
     case Sub(Sub(expr1, c: Cons), expr2) => Sub(Sub(expr1, expr2), c).simplify()

     // COMMUTATIVITY WITH CONSTS
     // c1*(c2*expr) = (c1*c2)*expr)
     case Mul(Cons(l1), Mul(Cons(l2), expr)) => Mul(Cons(l1 * l2), expr).simplify()
     // expr1*(c*expr) = c*(expr1*expr2)
     case Mul(expr1, Mul(c: Cons, expr2)) => Mul(c, Mul(expr1, expr2)).simplify()

     // ASSOCIATIVITY
     // a + (b+c) = (a+b)+c
     case Add(a, Add(b, c)) => Add(Add(a, b), c).simplify()

     // a + (b-c) = (a+b) - c
     case Add(a, Sub(b, c)) => Sub(Add(a, b), c).simplify()

     // COMMUTATIVITY
     // a*(b*c) = (a*b)*c
     case Mul(a, Mul(b, c)) => Mul(Mul(a, b), c).simplify()

     // a - (b + c) = (a-b)-c
     case Sub(a, Add(b, c)) => Sub(Sub(a, b), c).simplify()
     // a - (b - c) = (a-b)+c
     case Sub(a, Sub(b, c)) => Add(Sub(a, b), c).simplify()

     case Mul(a, b) => Mul(a.simplify(), b.simplify())
     case Add(a, b) => Add(a.simplify(), b.simplify())
     case Sub(a, b) => Sub(a.simplify(), b.simplify())

     case _ => this

   }
 }

  override def toString: String = {
    this match {
      case LpInteger(name) => name
      case Cons(v) => v.toString
      case Add(l,r) => "("+ l.toString + " + " + r.toString +")"
      case Mul(l,r) =>  "("+l.toString + " * " + r.toString +")"
      case Sub(l,r) =>  "("+l.toString + " - " + r.toString +")"
      case Minus(e) => "!-"+e.toString
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

case class Cons(value: Int) extends LpExpression

case class Add(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class Mul(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class Sub(lexpr: LpExpression, rexpr: LpExpression) extends LpExpression

case class Minus(expr: LpExpression) extends LpExpression

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

  implicit def intToLpInteger(s: Int): Cons = Cons(s)
}

