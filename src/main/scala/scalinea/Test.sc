import scalinea.grammar.Add

sealed trait Exp
case class Sum(a: Exp, b: Exp) extends Exp
case class Lit(v: Int) extends Exp

val a = Sum(Sum(Lit(1), Lit(2)), Lit(4))

object problem {
  def underConstraints(exps: => List[Exp]) = {
    exps
  }
}

  def add(l: Exp)(implicit ls: List[Exp]) = {
    ls.+:(l)
  }

problem underConstraints {
  implicit var ls: List[Exp] = List()
  add(Sum(Lit(1), Lit(1)))
  add(Sum(Lit(12), Lit(12)))
}









