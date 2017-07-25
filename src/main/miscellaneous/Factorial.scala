package main.miscellaneous

/**
  * Created on 7/24/17.
  */
object Factorial extends App {

  def factorial(x : Int): Int = {
    def f1(acc: Int, y : Int): Int = y match {
      case 0 =>
        val stackTraceAsArray = Thread.currentThread.getStackTrace
        stackTraceAsArray.foreach(println)
        acc
      case _ => f1(acc*y, y-1)
    }
    f1(1, x)
  }

  def factorial2(x:Int): Int = x match {
    case 0 =>
      val stackTraceAsArray = Thread.currentThread.getStackTrace
      stackTraceAsArray.foreach(println)
      1
    case _ => x * factorial2(x-1)
  }

  println(s"Factorial of x : ${factorial2(6)}")
}
