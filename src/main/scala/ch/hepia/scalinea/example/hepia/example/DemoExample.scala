import SchoolSchedule._
object DemoExample extends App {
    val scheduler = new Scheduler()
    scheduler.loadData(new DemoLoader())
    scheduler.showData
    scheduler.showPreferences("Cavat")
    scheduler.showLectures("Cavat")
    scheduler.showDailyHoursLimit
    scheduler.showIncompatibilities("Compilation 1")
    scheduler.addIncompatibility("Compilation 1", "POO Java 1")
    scheduler.showIncompatibilities("Compilation 1")
    scheduler.solve
    scheduler.softenConstraints("Cavat")
    scheduler.solve
    scheduler.showSolution
    scheduler.showSchedule("Cavat")
    scheduler.addDailyHoursLimit(7, "Cavat")
    scheduler.showDailyHoursLimit
    scheduler.addBreak(5)
    scheduler.solve
    scheduler.showSchedule("Cavat")
    scheduler.swapLectures("Web 1", "POO Java 1")
    scheduler.showSchedule("Cavat")
    scheduler.checkSolution
    scheduler.showSolution
    scheduler.exportSchedule("Cavat")
    scheduler.exportSchedule("Albuquerque")
    scheduler.exportSchedule("501")
    scheduler.exportSchedule("502")
}