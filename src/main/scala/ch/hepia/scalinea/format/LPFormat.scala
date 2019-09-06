package ch.hepia.scalinea
package format

import ch.hepia.scalinea.clause.Sign._
import ch.hepia.scalinea.clause.{Clause, Sign, Terms, CVar, Vars}
import ch.hepia.scalinea.dsl.System
import ch.hepia.scalinea.dsl.System.{Maximize, Minimize}


object LPFormat extends Format[Iterable[String]] {

  trait LpFormattable {
    def toLp: String
  }

  implicit class SignLpFormattable(s: Sign) extends LpFormattable {
    override def toLp: String = s match {
      case Eq => "="
      case NonEq => "!="
      case BigEq => ">="
      case LessEq => "<="
      case Big => ">"
      case Less => "<"
    }
  }

  implicit class VarsLpFormattable(vs: Vars) extends LpFormattable {
    override def toLp: String = {
      vs.sortedVar.map {
        v =>
          val exponent = vs.value (v).value
          if (exponent == 1)
            v.symbol
          else
            v.symbol + " ^ " + exponent.toString
      }.mkString (" ")
    }
  }

  implicit class TermsLpFormattable(ts: Terms) extends LpFormattable {
    override def toLp: String = {
      // FIXME: non linear vars are treated as quadratic vars
      val (linearVars, quadVars) = ts.sortedVars.partition(vars => vars.isLinear)

      val toStr: List[Vars] => List[String] =
        _.map(vars => ts.terms(vars).value.toString + " " + vars.toLp)

      val linearVarsExpr = toStr(linearVars).mkString(" + ")
      val quadVarsExpr = if (quadVars.isEmpty) "" else "[ " + toStr(quadVars).mkString(" + ") + " ]"
      val optionalAdd = if (linearVars.isEmpty || quadVars.isEmpty) "" else " + "
      linearVarsExpr + optionalAdd + quadVarsExpr
    }
  }

  def apply( system: clause.System ):  Output[Iterable[String]] = {
    val clauses: List[Clause] = system.constraints
    val goal: System.GoalTerms = system.goal
    val vars: Set[clause.Var] = system.vars

    // Objective section
    val goalLp: String = goal match {
      case Maximize(terms) => "Maximize\n obj: " + terms.toLp
      case Minimize(terms) => "Minimize\n obj: " + terms.toLp
    }

    // Constraints section
    val lps: List[String] = "Subject To" :: clauses.map {
      case Clause(ts,sign) => {

        // If constant exists in terms, put it on the right side of que constraint equation
        ts.terms.get( Vars(Map()) ) match {
          case Some( v ) =>
            Terms(ts.terms - Vars(Map())).toLp + " " +
              sign.toLp + " " + -v.value

          case None =>
            ts.toLp + " " +
              sign.toLp + " 0"
        }
      }
    }.zipWithIndex.map {
      case (line, index) if line.contains("[") => " qc" + index + ": " + line // quadratic
      case (line, index) => " c" + index + ": " + line
    }

    // Bound Section
    val bounds: List[String] = "Bounds" :: vars.toList.filter{
      case v@CVar(_,_,_) if v.isBounded => true
      case _ => false
    }.map {
      case CVar(symbol, Some(min), Some(max)) => min + " <= " + symbol + " <= " + max
      case CVar(symbol, None, Some(max)) => symbol + " <= " + max
      case CVar(symbol, Some(min), None) => min + " <= " + symbol
      case _ => throw new IllegalStateException() // Never happened due to the filter
    }.map( " " + _)

    // TODO: Generals (For integer vars)

    val end: String = "End"

    Success(goalLp :: lps ++ (bounds :+ end), Nil)

  }

}

