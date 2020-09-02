import SchoolSchedule._
object ItiExample extends App {
    //On prend en compte les 15, premiers enseignants et leurs cours
    val scheduler = new Scheduler()
    scheduler.loadData(new ItiLoader(3))
    scheduler.showData
    scheduler.solve
    scheduler.showSolution
}