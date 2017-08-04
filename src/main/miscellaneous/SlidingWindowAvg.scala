package main.miscellaneous

/**
  * Created on 8/4/17.
  *
  * Complexity : O(n) where n is the elements and
  *             does it depends on the windowsize??
  */
object SlidingWindowAvg extends App {

  def slidingAverage(l: List[Int], windowSize: Int) : List[Int] = {
    def f1(temp: List[Int], result: List[Int]): List[Int] =
      temp match {
        case List() => result
        case x :: xs if temp.length >= windowSize && windowSize > 0 =>
          f1(xs, result :+ (x + f2(xs, windowSize - 1, 0))/windowSize)
        case x :: xs if windowSize > 0 => result
        case _ => throw new IllegalArgumentException
      }

    //this calculates the sum for a dynamic window value
    def f2(y: List[Int], size: Int, sum: Int) : Int = (size,y) match {
      case (d, x :: xs) if d > 0 => f2(xs, size - 1, sum + x)
      case _ => sum
    }

    f1(l, List.empty[Int])
  }

  val input = List(1,2,3,4,5,6)
  println(s"Input List : $input")
  println(s"Output List : ${slidingAverage(input, 2)}")
  println(s"Output List : ${slidingAverage(input, 4)}")
  println(s"Output List : ${slidingAverage(input, 3)}")
  println(s"Output List : ${slidingAverage(input, 5)}")
  println(s"Output List : ${slidingAverage(input, 1)}")
  println(s"Output List : ${slidingAverage(input, 0)}")
  println(s"Output List : ${slidingAverage(input, 6)}")

}
