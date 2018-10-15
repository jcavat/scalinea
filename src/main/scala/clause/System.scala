package ch.hepia.scalinea
package clause

import format.{Format,Output}

case class System( constraints: List[Clause], goal: Terms ) {

  def exportTo[Out]( fmt: Format[Out] ): Output[Out] = fmt(this)

}
