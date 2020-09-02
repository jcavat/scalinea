package src.data

case class Teacher(name: String, preferences:Map[Period, Int], var lectures: List[Lecture]){
    private var _maxDailyPeriods: Int = 10
    private var _softenedConstraints: Boolean = false

    def setMaxDailyPeriods(n: Int) = _maxDailyPeriods = n
    def maxDailyPeriods() = _maxDailyPeriods
    def softenConstraints() = _softenedConstraints = true
    def strengthenConstraints() = _softenedConstraints = false
    def isConstraintSoftened() = _softenedConstraints
    override def toString() : String = { return name }
}