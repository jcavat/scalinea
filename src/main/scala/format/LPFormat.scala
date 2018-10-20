package ch.hepia.scalinea
package format

import ch.hepia.scalinea.clause.Sign._
import ch.hepia.scalinea.clause.{Clause, Sign, Terms, Vars}



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
      vs.sortedVars.map {
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
      val (linearVars, quadVars) = ts.sortedVars.partition(vars => vars.isLinear)

      val toStr: List[Vars] => List[String] =
        _.map(vars => ts.terms(vars).value.toString + " " + vars.toLp)

      val linearVarsExpr = toStr(linearVars).mkString(" + ")
      val quadVarsExpr = if (quadVars.isEmpty) "" else " [ " + toStr(quadVars).mkString(" + ") + " ] "

      linearVarsExpr + quadVarsExpr
    }
  }

  def apply( system: clause.System ):  Output[Iterable[String]] = {
    val clauses = system.constraints

    val lps: List[String] = clauses.map {
      case Clause(ts,sign) => {

        // If constant exists in terms, put it on the right side of que constraint equation
        ts.terms.get( Vars(Map()) ) match {
          case Some( v ) =>
            Terms(ts.terms - Vars(Map())).toLp + " " +
              sign + " " + -v.value

          case None =>
            ts.toLp + " " +
              sign.toLp + " 0"
        }
      }
    }

    Success(lps, Nil)

  }

}

