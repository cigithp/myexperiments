package main.miscellaneous

/**
  * Created on 8/25/17.
  */
object Power extends App {
  def myPow(x: Double, n: Int): Double = {
    def f1(acc: Double, a: Int ): Double = a match {
      case y if y > 1 => f1(acc * x, a-1)
      case _ => acc
    }
    n match {
      case 0 => 1.0
      case 2 => x * x
      case y if y % 2 == 0 => f1(x*x, n/2)
      case y if y < 0 => f1(1/x,-n)
      case _ => x * f1(x*x, n/2)
    }
  }

  println(s"POWER :: (2,3) :: ${myPow(2, 3)}")

  println(s"POWER :: (2.56783,5) :: ${myPow(2.56783, 5)}")

  println(s"POWER :: (2.56783,0) :: ${myPow(2.56783, 0)}")

  println(s"POWER :: (-2.56783,3) :: ${myPow(-2.56783, 3)}")
}
