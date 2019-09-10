package ch.hepia.scalinea
package format

import ch.hepia.scalinea.clause.Sign._
import ch.hepia.scalinea.clause._
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


  def apply(system: clause.System ):  Output[Iterable[String]] = {

    val goalLp: String = goalLpSection(system)

    val lps: List[String] = constraintsLpSection(system)

    val bounds: List[String] = boundsLpSection(system)

    val generals: List[String] = generalsLpSection(system)

    val binaries: List[String] = binariesLpSection(system)

    val end: String = "End"

    val finalLp = goalLp :: lps ++ (bounds ::: ((generals ::: binaries) :+ end))
    val warnings = if(finalLp.exists( p => p.length > 255)) {
      List("Line length is 255 char for some solvers, please check your solution carefully")
    }else{
      Nil
    }
    Success(finalLp, warnings)

  }


  def goalLpSection(system: clause.System): String = {
    system.goal match {
      case Maximize(terms) => "Maximize\n obj: " + terms.toLp
      case Minimize(terms) => "Minimize\n obj: " + terms.toLp
    }
  }
  def constraintsLpSection(system: clause.System): List[String] = {
    val constraints = system.constraints.map {
      case Clause(ts, sign) => {

        // If constant exists in terms, put it on the right side of que constraint equation
        ts.terms.get(Vars(Map())) match {
          case Some(v) =>
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
    if (constraints.isEmpty) Nil else "Subject To" :: constraints
  }

  def boundsLpSection(system: clause.System): List[String] = {
    val bounds = system.vars.filter{
      case v@ContinuousVar(_,_,_) if v.isBounded => true
      case v@IntegerVar(_,_,_) if v.isBounded => true
      case _ => false
    }.map {
      case cv@ContinuousVar(symbol,_,_) if cv.isFree => symbol + " free "
      case ContinuousVar(symbol, Some(min), Some(max)) => {
        val minSym = if (min == Double.NegativeInfinity) "-Inf" else min.toString
        val maxSym = if (max == Double.PositiveInfinity) "Inf" else max.toString
        minSym + " <= " + symbol + " <= " + maxSym
      }
      case ContinuousVar(symbol, None, Some(max)) => {
        val maxSym = if (max == Double.PositiveInfinity) "Inf" else max.toString
        symbol + " <= " + maxSym
      }
      case ContinuousVar(symbol, Some(min), None) => {
        val minSym = if (min == Double.NegativeInfinity) "-Inf" else min.toString
        minSym + " <= " + symbol
      }
        // minOp and maxOp cannot be both None
      case IntegerVar(symbol, minOp, maxOp) => {
        val minBound = minOp.map( _.toString + " <= " ).getOrElse("")
        val maxBound = maxOp.map(" <= " + _.toString ).getOrElse("")
        minBound + symbol + maxBound
      }
      case _ => throw new IllegalStateException() // Never happened due to the filter
    }.map( " " + _).toList

    if (bounds.isEmpty) Nil else "Bounds" +: bounds
  }
  def generalsLpSection(system: System): List[String] = {
    val vars = system.vars.filter {
      case iv@IntegerVar(_, _, _) => true
      case _ => false
    }.map {
      case IntegerVar(symbol, _, _) => symbol
      case _ => throw new IllegalStateException() // Never happened due to the filter
    }.mkString(" ")

    if (vars.isEmpty) Nil else List("General", " " + vars)

  }
  def binariesLpSection(system: System): List[String] = {
    val vars = system.vars.filter {
      case ib@BinaryVar(_) => true
      case _ => false
    }.map {
      case BinaryVar(symbol) => symbol
      case _ => throw new IllegalStateException() // Never happened due to the filter
    }.mkString(" ")

    if (vars.isEmpty) Nil else List("Binary", " " + vars)
  }

}

