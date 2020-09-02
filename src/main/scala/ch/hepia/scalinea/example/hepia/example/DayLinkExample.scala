import SchoolSchedule._
object DayLinkExample extends App {
    //On prend en compte les 10, premiers enseignants et leurs cours
    val scheduler = new Scheduler()
    scheduler.loadData(new TmpLoader())
    scheduler.showData
    scheduler.addBreak(4)
    scheduler.addLink("f", "e")
    scheduler.solve
    scheduler.showSchedule("Cavat")
    scheduler.showData
    scheduler.setDayLinkPonderation(10)
    scheduler.solve
    scheduler.showSchedule("Cavat")
    scheduler.exportSchedule("Cavat")
} 
