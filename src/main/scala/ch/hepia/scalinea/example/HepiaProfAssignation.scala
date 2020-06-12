package ch.hepia.scalinea.example


import ch.hepia.scalinea.dsl.{BVar, Constr, Expr, Ops}
import ch.hepia.scalinea.format.{Output, Success}
import ch.hepia.scalinea.solver.{CbcLpSolver, LPResult, Solution, Solver}
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Ops._

import scala.collection.mutable
import scala.reflect.runtime.universe._

case class ProfId(id: String)
case class ClassRoomId(id: String)
case class StudentId(id: String)
case class LectureId(id: String)
case class PeriodId(id: String)


class Manager() {

  private val incompatProfs: mutable.Map[ProfId, mutable.Buffer[ProfId]] = mutable.Map()
  private val incompatLectures: mutable.Map[LectureId, mutable.Buffer[LectureId]] = mutable.Map()
  private val profAssignment: mutable.Map[ProfId, mutable.Buffer[LectureId]] = mutable.Map()
  private val profPref : mutable.Map[ProfId, mutable.Map[PeriodId, Int]] = mutable.Map()

  private val profs: mutable.ListBuffer[ProfId] = mutable.ListBuffer()
  private val lectures: mutable.ListBuffer[LectureId] = mutable.ListBuffer()
  private val days = mutable.ListBuffer()

  def addIncompatibilities(prof1: ProfId, prof2: ProfId): Unit = {
    def addIncompatibilities(prof1: ProfId, prof2: ProfId): Unit = {
      incompatProfs.updateWith(prof1){
        case Some(profs) => Some(profs.append(prof2))
        case None => Some(mutable.Buffer(prof2))
      }
    }
    addIncompatibilities(prof1, prof2)
    addIncompatibilities(prof2, prof1)
  }

  def addIncompatibilities(lecture1: LectureId, lecture2: LectureId): Unit = {
    def addIncompatibilities(lecture1: LectureId, lecture2: LectureId): Unit = {
      incompatLectures.updateWith(lecture1){
        case Some(profs) => Some(profs.append(lecture2))
        case None => Some(mutable.Buffer(lecture2))
      }
    }
    addIncompatibilities(lecture1, lecture2)
    addIncompatibilities(lecture2, lecture1)
}

  def showPreferences(): Unit = {
    println(profPref)
    profPref.foreach{ case (profId, prefs) => {
      println("preferences of " + profId)
      prefs.foreach{ case (period, w) => println(f"period: $w)")}
    }}
  }
  def showIncompatibilities(): Unit = {
    incompatLectures.foreach { case (l, ls) => {
      println("lecture " + l.id + " incompatible with:")
      ls.foreach( l2 => println(" - " + l2.id))
    }}
    incompatProfs.foreach { case (p, ps) => {
      println("prof " + p.id + " incompatible with:")
      ps.foreach( p2 => println(" - " + p2.id))
    }}
  }

  def addPreference(prof: ProfId, period: PeriodId, weights: Int): Unit = {
    profPref.updateWith(prof){
      case Some(ponderations) => Some(ponderations.addOne( period -> weights))
      case None => Some(mutable.Map( period -> weights ))
    }
  }

  def add(lecture: LectureId): Unit = lectures.addOne(lecture)

  def add(prof: ProfId): Unit = profs.addOne(prof)

  def add[T](as: List[T])(implicit tag: TypeTag[T]): Unit = tag.tpe match {
    /* trick to override a generic argument, avoiding type erasure */
    case t if t =:= typeOf[ProfId] => as.asInstanceOf[List[ProfId]].foreach( add )
    case t if t =:= typeOf[LectureId] => as.asInstanceOf[List[LectureId]].foreach( add )
    case _ => println("none")
  }

  def assign(prof: ProfId, lecture: LectureId): Unit = {
    profAssignment.updateWith(prof){
      case Some(lectures) => {
        lectures.foreach( addIncompatibilities(_, lecture) )
        Some( lectures.append(lecture) )
      }
      case None => Some(mutable.Buffer(lecture))
    }
  }

 // def assign(student: StudentId, lecture: LectureId): Unit = ???

}


object HepiaProfAssignation extends App {

  val profs = List(ProfId("joel"), ProfId("paul"), ProfId("p3"), ProfId("p4"), ProfId("p5"), ProfId("p6"), ProfId("p7"))
  val lectures = List(LectureId("poo"), LectureId("sbd"), LectureId("sru"), LectureId("lecture4"), LectureId("lecture5"))
  val periods = List(PeriodId("mo"), PeriodId("tu"), PeriodId("we"), PeriodId("th"), PeriodId("fr"))

  val m = new Manager()

  m.add(profs)
  m.add(lectures)
  m.add(periods)

  m.addIncompatibilities(ProfId("joel"), ProfId("paul"))
  m.addIncompatibilities(ProfId("joel"), ProfId("nabil"))
  m.showIncompatibilities()
  m.assign(ProfId("joel"), LectureId("poo"))
  m.assign(ProfId("joel"), LectureId("sbd"))
  m.assign(ProfId("paul"), LectureId("poo"))
  m.assign(ProfId("paul"), LectureId("sru"))
  m.showIncompatibilities()

  m.addPreference(ProfId("joel"), PeriodId("mo"), 3)
  m.addPreference(ProfId("joel"), PeriodId("mo"), 3)
  m.showPreferences();



}
