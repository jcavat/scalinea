package SchoolSchedule
import src.data._
import ch.hepia.scalinea.dsl.{BVar, Constr, Expr, Ops}
import ch.hepia.scalinea.format.{Output, Success}
import ch.hepia.scalinea.solver.{CbcLpSolver, GurobiSolver, LPResult, Solution, Solver}
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Ops._

class Schedule {
  var rooms: List[Room] = List()
  var teachers: List[Teacher] = List()
  var lectures: List[Lecture] = List()
  var students: List[Student] = List()
  var periods: List[Period] = List()
  var workDays: List[WorkDay] = List()

  var dDayLink: Double = 0

  var lectureToPeriods : Map[Lecture, List[Period]] = Map()
  var lectureToRoom : Map[Lecture, Room] = Map()

  var periodBreaks: List[Int] = List()

  var incompatibilities: List[List[Lecture]] = List()
  var links: List[List[Lecture]] = List()

  def teacher(l: Lecture) : Teacher = { 
    (for{
      t <- teachers
      if(t.lectures.contains(l))} yield t).head
  }

  def students(l: Lecture) : List[Student] = {
    for{
      s <- students
      if(s.subscriptions.contains(l))
    } yield s
  }
  
  def incompatibleLectures(l: Lecture): List[Lecture] = {
    // Les cours donnés par le même enseignant
    var inc_l = for { li <- teacher(l).lectures if (l !=  li) } yield li
    //Les cours suivis par les mêmes élèves
    for{ 
      s <- students(l)
      sl <- s.subscriptions 
      if(l != sl && !inc_l.contains(sl))
    } yield inc_l = inc_l :+ sl
    // Les cours de la incompatibles
    for{ 
      incompatibleLectures <- incompatibilities
      li <- incompatibleLectures
      if( incompatibleLectures.contains(l) && l != li && !inc_l.contains(li))
    } yield inc_l = inc_l :+ li
    inc_l
  }

  def linkedLectures(l: Lecture): List[Lecture] = {
    var link_l: List[Lecture] = List()
    for {
      linkedLectures <- links
      ll <- linkedLectures
      if (linkedLectures.contains(l) && l != ll && !link_l.contains(ll))
    } yield link_l = link_l :+ ll
    link_l
  }

  def periodsForLecture(firstP: Period, l:Lecture): List[Period] = {
    val pfl = for { i <- 0 until l.duration } yield periods(periods.indexOf(firstP)+i) 
    pfl.toList
  }

  def solve(): ScheduleSolution = {
    println("System build start")
    val t0 = System.nanoTime
    val x: Map[Lecture, Map[Period, BVar]] = lectures.map( l => l -> periods.map( p => p -> BVar(s"${l.varName}_is_given_at_${p}")).toMap).toMap
    val y: Map[Lecture, Map[Room, BVar]] = lectures.map(l => l -> rooms.map(r => r -> BVar(s"${l.varName}_is_given_in_${r}")).toMap).toMap


    val a = {
      dsl.System.define
      // (A) Pour chaque cours, la somme des periodes attribuées est égale à la durée du cours
      .constraints( forAll(lectures )( l => sum(periods)( p => x(l)(p)) === l.duration) )
      // (B) Pour chaque periode la somme des classes incompatibles attribuées est inférieure ou égale à 1
      .constraints(
        for{
          l <- lectures
          li <- incompatibleLectures(l)
          p <- periods
        } yield (x(l)(p) + x(li)(p) <= 1)
      )
      //(C.1) Si un cours n'est pas donné à une periode, et est donné à la suivante, alors il est donné aux periodes consécutives
      .constraints(
        for{
          l <- lectures
          i <- 1 until periods.size
          if(i + l.duration < periods.size) 
          p <- periodsForLecture(periods(i), l)
        } yield (1-x(l)(periods(i-1))) + x(l)(periods(i)) -1 <= x(l)(p)
      )
      //(C.2) si un cours est donné en première heure, alors il est donné aux periodes consécutives
      .constraints(
        for{
          d <- workDays
          l <- lectures
          p <- periodsForLecture(d.periods(0), l)
        } yield (x(l)(d.periods(0))) <= x(l)(p)
      )
      //(D) Chaque cours doit avoir lieu dans une salle
      .constraints( forAll(lectures)( l => sum(rooms)( r => y(l)(r)) === 1 ) )
      //(E) chaque enseignant doit faire au maximum son nombre d'heures maximum par jour
      .constraints(
        for{
          d <- workDays
          t <- teachers
        } yield sum(d.periods)(p => sum(t.lectures)(l => x(l)(p))) <= t.maxDailyPeriods
      )
      //(F) A chaque periode, une salle ne peut accueillir qu'un cours
      .constraints(
        for{
            r <- rooms
            p <- periods
            li <- lectures
            lj <- lectures
            if( li != lj)
          } yield (x(li)(p) + x(lj)(p)) + y(li)(r) -2 <= (1- y(lj)(r))
      )
      //(G) Les periodes avec une preference de 0 sont bloquées, à moins que les préférences soient adoucies
      .constraints(
          for{
            l <- lectures
            p <- periods
            if(teacher(l).preferences(p) == 0 && !teacher(l).isConstraintSoftened())
          } yield x(l)(p) === 0
      )
      //(H) Les pauses de doivent être respectées
      .constraints(
        for{
          break <- periodBreaks
          l <- lectures
          d <- workDays
          if(break < d.periods.size)
        } yield x(l)(d.periods(break)) === 0
      ) 
      //(I) Les assignations des cours doivent être respectés
      .constraints(
        for{
          (l, ps) <- lectureToPeriods
          p <- ps
        } yield x(l)(p) === 1
      )
      //(J) Les assignations des salles doivent être respectées
      .constraints(
        for{
          (l, r) <- lectureToRoom
        } yield (y(l)(r) === 1)
      )
      .maximize(
        //la somme des préférences de chaque enseignant pour l'horaire lui étant attribué
        sum(for{
            l <- lectures
            p <- periods
          } yield teacher(l).preferences(p) * x(l)(p))
        //Le nombre de cours liés ayant lieu le même jour
        + dDayLink * sum(for {
          d <- workDays
          p <- periods
          l <- lectures
          ll <- linkedLectures(l)
        } yield if(d.periods.contains(p)) x(l)(p) + x(ll)(p) else x(l)(p) - x(ll)(p))
      )
    }
    println("X")
    val system = a.build
    println("Y")

    val buildDuration = (System.nanoTime - t0) / 1e9d
    println(s"System built in $buildDuration s")
    val solver: Solver = GurobiSolver
    //OutputHandler.hide
    println("System solving start")
    val result: Output[LPResult] = solver.solve(system)
    //OutputHandler.show
    
    var solutionLectureToPeriod: Map[Lecture, List[Period]] = Map()
    var solutionLectureToRoom: Map[Lecture, Room] = Map()

    result match{
      case Success(s: Solution, _) => {
        if(s.isOptimal) {
          println("Solver found optimal solution.")
          for( l <- lectures ; p <- periods ) {
            if(s(x(l)(p)))
              if (solutionLectureToPeriod.contains(l)) solutionLectureToPeriod += l -> (solutionLectureToPeriod(l) :+ p) else solutionLectureToPeriod += l -> List(p)
        }
        for(l <- lectures; r <- rooms)
          if(s(y(l)(r))) solutionLectureToRoom += l -> r
        }
      }
      case _ => println("ERROR")
    }
    ScheduleSolution(solutionLectureToPeriod, solutionLectureToRoom)
  }
}