package ch.hepia.scalinea
package clause

import ch.hepia.scalinea.dsl.System.HasGoal
import format.{Format, Output}

case class System( constraints: List[Clause], goal: HasGoal) {

  def exportTo[Out]( fmt: Format[Out] ): Output[Out] = fmt(this)

}
