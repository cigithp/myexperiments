package main.algorithms.sorting

/**
  * Time complexities :
  * Best case
  * Worst case
  *
  * Space Complexities :
  * Best case
  * Worst case
  *
  * */

object InsertionSort extends App {

  def iSort(xs: List[Int]): List[Int] =
    xs match {
      case List() => Nil
      case x :: xs1 => insert(x,iSort(xs1))
    }

  def insert(x: Int, xs: List[Int]): List[Int] =
    xs match {
      case List() => List(x)
      case y :: ys => if (x <= y) x :: xs
                      else y :: insert(x, ys)
    }

  val inputList = List(1, 4, 7, 3, 2, 9)
  println(s"Input List : ${inputList}")
  println(s"Sorted List : ${iSort(inputList)}")
}