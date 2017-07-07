import scalinea.grammar.Add

sealed trait Exp
case class Sum(a: Exp, b: Exp) extends Exp
case class Lit(v: Int) extends Exp


case class Problem(ls: List[Exp], objective: Option[Exp]){
  def get() = (ls, objective)
}

object Problem {


  var ls: List[Exp] = List()
  var objecti: Option[Exp] = None


  case object ConstraintOk {
    def apply(s: String) = {
      ls :+ Lit(2)
    }
  }

  def underConstraints(exps: => Unit) = {
    ls = List()
    objecti = None
    exps
    Problem(ls, objecti)
  }


  sealed trait Action
  case object add extends Action

  sealed trait ActionFun
  case object maximize extends ActionFun

  case class AddBuilder(action: Action) {
    def constraint(e: Exp) = {
      ls = ls :+ e
    }
  }

  case class FunBuilder(action: ActionFun) {
    def obj(e: Exp) = {
      objecti = Some(e)
    }
  }

  //implicit def strToExp(s: String) = Lit(0)
  implicit def actionToFunBuilder(action: ActionFun) = FunBuilder(action)
  implicit def actionToActionBuilder(action: Action) = AddBuilder(action)
}

import Problem._

Problem underConstraints {
  add constraint Sum(Lit(1),Lit(2))
  add constraint Sum(Lit(1),Lit(2))
  maximize obj Lit(1)
}

Problem underConstraints {
  add constraint Lit(1)
}

