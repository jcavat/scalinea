package ch.hepia.scalinea
package format

import ch.hepia.scalinea.clause.{Clause, Sign, Terms, Vars}
import ch.hepia.scalinea.util.LpFormat


object LPFormat extends Format[Iterable[String]] {

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

