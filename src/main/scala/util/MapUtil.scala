package ch.hepia.scalinea.util

object MapUtil {

  def mergeOpt[K,V]( lhs: Map[K,V], rhs: Map[K,V], f: (V,V)=>Option[V] ): Map[K,V] = {
    (lhs.keySet ++ rhs.keySet).map { k =>
      (lhs.get(k), rhs.get(k)) match {
        case (Some(v1), Some(v2)) => k -> f(v1,v2)
        case (None, Some(v)) => k -> Some(v)
        case (Some(v), None) => k -> Some(v)
        case (None, None) => k -> None // Unreachable code
      }
    }.collect{ case (k, Some(v)) => (k,v) }.toMap

  }

}

