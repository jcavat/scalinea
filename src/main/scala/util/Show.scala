package ch.hepia.scalinea.util

trait Show[A] {
  def asString( a: A ): String
  def print( a: A ): Unit = println( asString(a) )
}

object Show {
  def apply[A]( implicit s: Show[A] ) = s
  def instance[A]( f: A=>String ) = new Show[A] {
    def asString( a: A ) = f(a)
  }
}
