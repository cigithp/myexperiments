package main.miscellaneous

/**
  * Created on 8/19/17.
  */
object Solution extends App {
  def isMatch(s: String, p: String): Unit =
    for(i <- 0 until p.length - 1; j <- 0 until s.length - 1)
      p(i) match {
        case '*' => i
        case '.' => i
        case _ if p(i) == s(j) => j
        case _ => false
      }







//    if (containsAsterik(p)) true
//    else if (containsDot(p) && checkString(s, p.split('.').toList)) true
//    else if (s.contentEquals(p)) true
//    else false
//
//  def checkString(s:String, stringArray: List[String]) : Boolean = stringArray match {
//    case List() => false
//    case x :: xs => if (s.contains(x.takeRight(1))) true else checkString(s, xs)
//  }
//
//  def containsAsterik(p: String): Boolean = if (p.contains('*')) true else false
//
//  def containsDot(p: String): Boolean = if (p.contains('.')) true else false

  //println(s"RESULT ::: ${isMatch("aa", "aa.")}")

}
