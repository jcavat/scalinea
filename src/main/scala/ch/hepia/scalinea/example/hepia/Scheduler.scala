package SchoolSchedule
import src.data._
class Scheduler{
    var schedule = new Schedule()
    var solution = ScheduleSolution(Map(), Map())
    var loader: DataLoader = new DemoLoader()
    var exporter: Exporter = new HtmlExporter()

    private def loadRooms(data: DataLoader) = schedule.rooms = data.loadRooms()
    private def loadTeachers(data: DataLoader) = schedule.teachers = data.loadTeachers()
    private def loadLectures(data: DataLoader) = schedule.lectures = data.loadLectures()
    private def loadStudents(data: DataLoader) = schedule.students = data.loadStudents()
    private def loadPeriods(data: DataLoader) = schedule.periods = data.loadPeriods()
    private def loadWorkDays(data: DataLoader) = schedule.workDays = data.loadWorkDays()
    def loadData(loader: DataLoader) = {loadRooms(loader); loadTeachers(loader); loadLectures(loader); loadStudents(loader); loadPeriods(loader); loadWorkDays(loader)}
    
    //inspect data
    def showData() = {showLectures; showRooms; showTeachers; showStudents}
    def showTeachers() = {println("Teachers:"); schedule.teachers.foreach(t => println("%1$32s".format(t)))}
    def showStudents() = {println("Students:"); schedule.students.foreach(s => println("%1$32s".format(s)))}
    def showRooms() = {println("Rooms:"); schedule.rooms.foreach(r => println("%1$32s".format(r)))}
    def showLectures()  = {println("Lectures:"); schedule.lectures.foreach(l => println("%1$32s".format(l)))}

    //finders
    private def findLecture(input: String) : Option[Lecture] = {
        val ls = schedule.lectures.filter(l => l.name == input)
        if(ls.isEmpty){ println(s"No lecture found for input : $input"); showLectures; return None }
        else return Some(ls.head)
    }
    private def findTeacher(input: String) : Option[Teacher] = {
        val ts = schedule.teachers.filter(t => t.name == input)
        if(ts.isEmpty){ println(s"No teacher found for input : $input"); showTeachers; return None }
        else return Some(ts.head)
    }
    private def findStudent(input: String) : Option[Student] = {
        val ss = schedule.students.filter(s => s.name == input)
        if(ss.isEmpty){ println(s"No student found for input : $input"); showStudents; return None }
        else return Some(ss.head)
    }

    //inspect context
    def showPreferences(input: String) : Unit = { val t = findTeacher(input); if(t.isDefined) showPreferences(t.get)}
    def showPreferences(t: Teacher) : Unit = pprint.pprintln(t.preferences)
    
    def showLectures(input: String) : Unit = {
        val ss = schedule.students.filter(s => s.name == input)
        val ts = schedule.teachers.filter(t => t.name == input)
        if(ss.isEmpty && ts.isEmpty) println(s"No teacher and no student found for input : $input")
        else if(!ss.isEmpty) ss.foreach(s => showLectures(s))
        else if(!ts.isEmpty) ts.foreach(t => showLectures(t))
    }

    def showLectures(s: Student) = pprint.pprintln(s.subscriptions)
    def showLectures(t: Teacher) = pprint.pprintln(t.lectures)

    def showIncompatibilities(input: String) : Unit = { val l = findLecture(input); if(l.isDefined) showIncompatibilities(l.get) }
    def showIncompatibilities(l: Lecture) = pprint.pprintln(schedule.incompatibleLectures(l))
    def showIncompatibilities() = pprint.pprintln(schedule.incompatibilities)

    def showLinks(input: String) : Unit = { val l = findLecture(input); if(l.isDefined) showLinks(l.get) }
    def showLinks(l: Lecture) = pprint.pprintln(schedule.linkedLectures(l))
    def showLinks() = pprint.pprintln(schedule.links)

    def showDailyHoursLimit() : Unit = schedule.teachers.foreach(t => showDailyHoursLimit(t))
    def showDailyHoursLimit(t: Teacher) : Unit = {val l = t.maxDailyPeriods; println(s"$t: $l")}
    def showDailyHoursLimit(ts: List[Teacher]) : Unit = ts.foreach(t => showDailyHoursLimit(t))
    def showDailyHoursLimit(input: String) : Unit = {val t = findTeacher(input); if(t.isDefined) showDailyHoursLimit(t.get)}
    def showDailyHoursLimit(inputs: String*) : Unit = {
        showDailyHoursLimit((for{
            i <- inputs
            t = findTeacher(i)
            if(t.isDefined)
        } yield t.get).toList)
    }

    // add constraints
    def addIncompatibility(input: String*) : Unit = { 
        addIncompatibility((for{
            i <- input
            l = findLecture(i)
            if(l.isDefined)
        } yield l.get).toList)
    }
    def addIncompatibility(lectures: List[Lecture]) : Unit = schedule.incompatibilities = schedule.incompatibilities :+ lectures

    def addLink(input: String*) : Unit = {
        addLink((for{
            i <- input
            l = findLecture(i)
            if(l.isDefined)
        } yield l.get).toList)
    }
    def addLink(lectures: List[Lecture]) = schedule.links = schedule.links :+ lectures

    def addBreak(period: Int) = schedule.periodBreaks = schedule.periodBreaks :+ period

    def addDailyHoursLimit(h: Int) : Unit = schedule.teachers.foreach(t => addDailyHoursLimit(h, t))
    def addDailyHoursLimit(h: Int, input: String): Unit = {val t = findTeacher(input); if(t.isDefined) addDailyHoursLimit(h, t.get)}
    def addDailyHoursLimit(input: String, h: Int): Unit = addDailyHoursLimit(h, input)
    def addDailyHoursLimit(h: Int, inputs: String*) : Unit = inputs.foreach(i => {val t = findTeacher(i); if(t.isDefined) addDailyHoursLimit(h, t.get)})
    def addDailyHoursLimit(h: Int, t: Teacher) : Unit = t.setMaxDailyPeriods(h)
    def addDailyHoursLimit(h: Int, ts: List[Teacher]) : Unit = ts.foreach(t => addDailyHoursLimit(h, t)) 
    def addDailyHoursLimit(t: Teacher, h: Int) : Unit = addDailyHoursLimit(h, t)
    def addDailyHoursLimit(ts: List[Teacher], h: Int) : Unit = addDailyHoursLimit(h, ts)

    def softenConstraints() : Unit = schedule.teachers.foreach(t => softenConstraints(t))
    def softenConstraints(input: String) : Unit = {val t = findTeacher(input); if(t.isDefined) softenConstraints(t.get)}
    def softenConstraints(input: String*) : Unit = input.foreach(i => softenConstraints(i))
    def softenConstraints(t: Teacher) = t.softenConstraints()
    def softenConstraints(ts: List[Teacher]): Unit = ts.foreach(softenConstraints(_))

    // Set a weight for weak constraints, the bigger the weight, the more the constraint matter
    def setDayLinkPonderation(d: Double) = {schedule.dDayLink = d}

    // Divides a lecture into two new lectures, part A and B and removes all data about previous lecture
    def splitLecture(l: Lecture) : Unit = {
        schedule.lectures = schedule.lectures.filter(_ != l)
        val l1 = Lecture(s"${l.name}(A)", l.duration / 2)
        val l2 = Lecture(s"${l.name}(B)", l.duration- l.duration / 2)
        val t = schedule.teacher(l)
        t.lectures = t.lectures :+ l1
        t.lectures = t.lectures :+ l2
        t.lectures = t.lectures.filter(_ != l)
        schedule.students(l).foreach(s => {
            s.subscriptions = s.subscriptions :+ l1
            s.subscriptions = s.subscriptions :+ l2
            s.subscriptions = s.subscriptions.filter(_ != l)
        })
        schedule.lectures = schedule.lectures :+ l1
        schedule.lectures = schedule.lectures :+ l2
        schedule.incompatibilities = schedule.incompatibilities.filter(!_.contains(l))
        schedule.links = schedule.links.filter(!_.contains(l))
        schedule.lectureToRoom = schedule.lectureToRoom - l
        schedule.lectureToPeriods = schedule.lectureToPeriods - l
    }
    def splitLecture(input: String) : Unit = { val l = findLecture(input); if(l.isDefined) splitLecture(l.get)}     

    def assign(t: Teacher, l: Lecture) : Unit = assign(l, t)
    def assign(l: Lecture, t: Teacher) = if(!t.lectures.contains(l)) t.lectures = t.lectures :+ l
    def assign(t: Teacher, ls: List[Lecture]) : Unit = assign(ls, t)
    def assign(ls: List[Lecture], t: Teacher): Unit = ls.foreach(assign(_ , t))
    def assign(l: Lecture, p: Period) = if(schedule.lectureToPeriods.contains(l) && !schedule.lectureToPeriods(l).contains(p)) schedule.lectureToPeriods += l -> (schedule.lectureToPeriods(l) :+ p) else schedule.lectureToPeriods += l -> List(p)
    def assign(l: Lecture, ps: List[Period]) : Unit = ps.foreach(assign(l, _))
    def assign(r: Room, l: Lecture) : Unit = assign(l, r)
    def assign(l: Lecture, r: Room) = schedule.lectureToRoom += l -> r

    // SolvingLPResult
    def solve() = solution = schedule.solve()
    
    // see solution
    def showSolution() = {schedule.rooms.foreach(r => showSchedule(r)); schedule.teachers.foreach(t => showSchedule(t)); schedule.students.foreach(s => showSchedule(s))}

    private def teacherSchedule(t: Teacher): Map[Lecture, List[Period]] = solution.x.filter((x) => schedule.teacher(x._1) == t)
    private def studentSchedule(s: Student): Map[Lecture, List[Period]] = solution.x.filter((x) => schedule.students(x._1).contains(s))
    private def roomSchedule(r: Room): Map[Lecture, List[Period]] = solution.x.filter((x) => solution.y(x._1) == r)

    def showSchedule(input: String) : Unit = {
        val ts = schedule.teachers.filter(t => t.name == input)
        val ss = schedule.students.filter(s => s.name == input)
        val rs = schedule.rooms.filter(r => r.id == input)
        if (ts.isEmpty && ss.isEmpty && rs.isEmpty) {println(s"No schedule found for input: $input")}
        else if(!ts.isEmpty) ts.foreach(t => showSchedule(t))
        else if(!ss.isEmpty) ss.foreach(s => showSchedule(s))
        else if(!rs.isEmpty) rs.foreach(r => showSchedule(r))
    }
    def showSchedule(t: Teacher) = (new TerminalExporter()).exportSchedule(t.name, teacherSchedule(t), schedule.workDays)
    def showSchedule(s: Student) = (new TerminalExporter()).exportSchedule(s.name, studentSchedule(s), schedule.workDays)
    def showSchedule(r: Room) = (new TerminalExporter()).exportSchedule(r.id, roomSchedule(r), schedule.workDays)

    // manage soution
    def swapLectures(i1:String, i2:String) : Unit = {val l1 = findLecture(i1); val l2 = findLecture(i2); if(l1.isDefined && l2.isDefined) swapLectures(l1.get, l2.get)}
    def swapLectures(l1: Lecture, l2:Lecture) = { val tmp = solution.x(l1); solution.x += l1 -> solution.x(l2); solution.x += l2 -> tmp}
    def checkSolution() = { schedule.lectureToPeriods = solution.x; schedule.lectureToRoom = solution.y; solve}

    //Save
    def save(saver: Saver, filename: String) = saver.saveVersion(filename, this)
    def load(loader: Loader, filename: String) = loader.loadVersion(filename, this)

    // export solution
    def exportSchedule(input: String): Unit = {
        val ts = schedule.teachers.filter(t => t.name == input)
        val ss = schedule.students.filter(s => s.name == input)
        val rs = schedule.rooms.filter(r => r.id == input)
        if (ts.isEmpty && ss.isEmpty && rs.isEmpty) {println(s"No schedule found for input: $input")}
        else if(!ts.isEmpty) ts.foreach(t => exportSchedule(t))
        else if(!ss.isEmpty) ss.foreach(s => exportSchedule(s))
        else if(!rs.isEmpty) rs.foreach(r => exportSchedule(r))
    }
    def exportSchedule(t: Teacher) : Unit = exporter.exportSchedule(t.name, teacherSchedule(t), schedule.workDays)
    def exportSchedule(s: Student) : Unit = exporter.exportSchedule(s.name, studentSchedule(s), schedule.workDays)
    def exportSchedule(r: Room) : Unit = exporter.exportSchedule(r.id, roomSchedule(r), schedule.workDays)
}