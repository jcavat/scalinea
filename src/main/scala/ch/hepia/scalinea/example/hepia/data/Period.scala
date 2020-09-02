package src.data

import Day._

case class Period(day: Day, number:Int){
    override def toString() : String = s"${day}_$number"
}