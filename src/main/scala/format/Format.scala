package ch.hepia.scalinea
package format

sealed trait Output[+T] {
  def warnings: List[String]
}
case class Failure( errors: List[String], warnings: List[String] ) extends Output[Nothing]
case class Success[T]( result: T, warnings: List[String] ) extends Output[T]

trait Format[+T] {
  def apply( system: clause.System ): Output[T]
}


