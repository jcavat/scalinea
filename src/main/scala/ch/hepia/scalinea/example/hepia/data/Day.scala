package src.data

object Day extends Enumeration {
  type Day = Value
  val Monday, Tuesday, Wednesday, Thursday, Friday = Value
  def dayText(s: String) : Day = s match {
      case "Monday" => Monday
      case "Tuesday" => Tuesday
      case "Wednesday" => Wednesday
      case "Thursday" => Thursday
      case "Friday" => Friday
  }
}