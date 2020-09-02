package src.data

case class Student(name: String, var subscriptions: List[Lecture]){
    override def toString() : String = { return name }
}