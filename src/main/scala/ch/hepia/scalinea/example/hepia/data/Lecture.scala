package src.data

case class Lecture(name: String, duration: Int){
    def this(name: String) = this(name, 2)
    override def toString() : String = name
    def varName = name.replaceAll("[^a-zA-Z0-9]", "")
}