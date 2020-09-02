package SchoolSchedule

object LoaderMode extends Enumeration {
  type LoaderMode = Value
  val LoadingWorkDays, LoadingRooms, LoadingLectures, LoadingStudents, LoadingTeachers, LoadingDDayDensity, LoadingDDayLink, LoadingLectureToPeriods, LoadingLectureToRoom, LoadingPeriodBreaks, LoadingIncompatibilities, LoadingLinks = Value
}
