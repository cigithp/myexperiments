package main.algorithms.sorting

/**
  * Created on 7/16/17.
  *
  * Divide and Conquer Principle
  *
  * Time complexities :
  * Best case
  * Worst case
  *
  * Space Complexities :
  * Best case
  * Worst case
  *
  * */

object MergeSort extends App {
  def mSort(less: (Int, Int) => Boolean)(xs: List[Int]): List[Int] = {

    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(mSort(less)(ys), mSort(less)(zs))
    }
  }

  val inputList = List(1, 4, 7, 3, 2, 9)
  println(s"Input List : ${inputList}")
  println(s"Sorted List : ${mSort((x: Int, y: Int) => x > y)(inputList)}")
}
