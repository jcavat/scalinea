package ch.hepia.scalinea.util

trait LpFormat[A] {
  def asString( a: A ): String
  def print( a: A ): Unit = println( asString(a) )
}

object LpFormat {
  def apply[A]( implicit s: LpFormat[A] ) = s
  def instance[A]( f: A=>String ) = new LpFormat[A] {
    def asString( a: A ) = f(a)
  }
}
