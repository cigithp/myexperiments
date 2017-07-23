package main.algorithms.sorting

/**
  * Created on 7/22/17.
  */
object SelectionSort extends App {

  def sort(list: List[Int]): List[Int] = {
    def sort1(xs: List[Int], accumulator: List[Int]): List[Int] = xs match {
      case List() => accumulator
      case _ =>
        val ys = maximum(xs)
        sort1(ys.tail, ys.head :: accumulator)
    }
    sort1(list, List())
  }

  val inputList = List(1, 4, 7, 3, 2, 9)
  println(s"Input List : ${inputList}")
  println(s"Sorted List : ${sort(inputList)}")
  println(s"Sorted List : ${ssort(inputList)}")


  //creates a new list with the minimum at the head position
  def minimum(xs:List[Int]): List[Int] = xs.tail.foldLeft(List(xs.head))((ys,x) => if(x < ys.head) x :: ys else ys.head :: x :: ys.tail)

  //creates a new list with the maximum at the head position
  def maximum(xs:List[Int]): List[Int] = xs.tail.foldLeft(List(xs.head))((ys,x) => if(x > ys.head) x :: ys else ys.head :: x :: ys.tail)

  //non tail recursive - a bad solution
  def ssort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) List()
    else{
      val ys = minimum(xs)
      if (ys.tail.isEmpty)
        ys
      else
        ys.head :: ssort(ys.tail)
    }
  }
}
