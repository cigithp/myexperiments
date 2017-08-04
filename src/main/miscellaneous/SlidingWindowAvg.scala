package main.miscellaneous

/**
  * Created on 8/4/17.
  */
object SlidingWindowAvg extends App {

  def slidingAverage(l: List[Int], windowSize: Int) : List[Int] = {
    def f1(temp: List[Int], result: List[Int]): List[Int] =
      temp match {
        case x :: xs if temp.length >= windowSize =>
          f1(xs, result :+ (x + xs.head + xs.tail.head)/windowSize)
        case x :: xs => result
      }
    f1(l, List.empty[Int])
  }

  val input = List(1,2,3,4,5,6)
  println(s"Input List : $input")
  println(s"Output List : ${slidingAverage(input, 3)}")

}
