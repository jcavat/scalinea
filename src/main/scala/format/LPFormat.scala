package ch.hepia.scalinea
package format

import ch.hepia.scalinea.clause.Sign._
import ch.hepia.scalinea.clause.{Clause, Sign, Terms, Vars}
import ch.hepia.scalinea.util.LpFormat


object LPFormat extends Format[Iterable[String]] {

  implicit val canExportSignToLp = LpFormat.instance[Sign]{
    case Eq => "="
    case NonEq => "!="
    case BigEq => ">="
    case LessEq => "<="
    case Big => ">"
    case Less => "<"
  }

  implicit val canExportVarsToLp = LpFormat.instance[Vars]{ vs =>
    vs.sortedVars.map{ v =>
      val exponent = vs.value(v).value
      if( exponent == 1 )
        v.symbol
      else
        v.symbol+" ^ "+exponent.toString
    }.mkString(" ")
  }

  implicit val canExportToTermsLp = LpFormat.instance[Terms]{ ts =>

    val (linearVars, quadVars) = ts.sortedVars.partition( vars => vars.isLinear )

    val toStr: List[Vars] => List[String] =
      _.map( vars => ts.terms(vars).value.toString + " " + LpFormat[Vars].asString(vars) )

    val linearVarsExpr = toStr(linearVars).mkString(" + ")
    val quadVarsExpr = if (quadVars.isEmpty) "" else " [ " + toStr(quadVars).mkString(" + ") + " ] "

    linearVarsExpr + quadVarsExpr

  }

  def apply( system: clause.System ):  Output[Iterable[String]] = {
    val clauses = system.constraints

    val lps: List[String] = clauses.map {
      case Clause(ts,sign) => {

        // If constant exists in terms, put it on the right side of que constraint equation
        ts.terms.get( Vars(Map()) ) match {
          case Some( v ) =>
            LpFormat[Terms].asString(Terms(ts.terms - Vars(Map()))) + " " +
              LpFormat[Sign].asString(sign) + " " + -v.value

          case None =>
            LpFormat[Terms].asString(ts) + " " +
              LpFormat[Sign].asString(sign) + " 0"
        }
      }
    }

    Success(lps, Nil)


  }

}

