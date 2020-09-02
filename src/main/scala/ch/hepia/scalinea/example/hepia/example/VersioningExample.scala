import SchoolSchedule._
object VersioningExample extends App {
    val scheduler = new Scheduler()
    scheduler.loadData(new TmpLoader())
    scheduler.addIncompatibility("c", "d", "e")
    scheduler.addIncompatibility("a", "f", "g")
    scheduler.addLink("a", "b")
    scheduler.addLink("e", "f")
    scheduler.addBreak(5)
    scheduler.addBreak(1)
    scheduler.assign(scheduler.schedule.lectures(0), scheduler.schedule.rooms(0))
    scheduler.assign(scheduler.schedule.lectures(1), scheduler.schedule.rooms(0))
    scheduler.assign(scheduler.schedule.lectures(2), scheduler.schedule.rooms(0))
    scheduler.assign(scheduler.schedule.lectures(3), scheduler.schedule.periods(0))
    scheduler.assign(scheduler.schedule.lectures(4), List(scheduler.schedule.periods(11), scheduler.schedule.periods(12)))
    scheduler.save(new SaverCSV(), "example_version_0.0.0")
    scheduler.load(new LoaderCSV(), "example_version_0.0.0")
}