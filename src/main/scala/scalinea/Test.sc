import scalinea.grammar.Add

sealed trait Exp
case class Sum(a: Exp, b: Exp) extends Exp
case class Lit(v: Int) extends Exp


object problem {

  var ls: List[Exp] = List()

  case object ConstraintOk {
    def apply(s: String) = {
      ls :+ Lit(2)
    }
  }

  def underConstraints(exps: => List[Exp]) = {
    exps
    ls
  }

  sealed trait Action
  case object add extends Action

  sealed trait ActionFun
  case object maximize extends ActionFun

  case class AddBuilder(action: Action) {
    def constraint(e: Exp): List[Exp] = {
      ls = ls :+ e
      ls
    }
  }

  case class FunBuilder(action: ActionFun) {
    def obj(e: Exp): List[Exp] = {
      ls = ls :+ e
      ls
    }
  }

  //implicit def strToExp(s: String) = Lit(0)
  implicit def actionToFunBuilder(action: ActionFun) = FunBuilder(action)
  implicit def actionToActionBuilder(action: Action) = AddBuilder(action)
}
import problem._


problem underConstraints {
  add constraint Sum(Lit(1),Lit(2))
  add constraint Sum(Lit(1),Lit(2))
  maximize obj Lit(1)
}

