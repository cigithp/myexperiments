package main.algorithms.sorting

/**
  * Created by pillaci on 7/22/17.
  *
  * Divide and Conquer Principle
  *
  * Time Complexities :
  * Best case
  * Worst case : O(N^2)
  * Average case : O(N log N)
  *
  * Space Complexities :
  * Best case :
  * Worst case :
  *
  *
  */
object QuickSort extends App {

  def sort(xs: Array[Int]) = {
    def swap(x: Int, y: Int) = {
      val temp = xs(x)
      xs(x) = xs(y)
      xs(y) = temp
    }
    def sort1(start: Int, end: Int): Array[Int] = {
      val pivot: Int = xs((start + end) / 2)
      var i = start
      var j = end
      while(i <= j){
        while(xs(i) < pivot) i+=1
        while(xs(j) > pivot) j -=1
        if(i <= j){
          swap(i,j)
          i+=1; j-=1
        }
      }
      if(start < j) sort1(start,j)
      if(j < end) sort1(i, end)
      xs
    }
    sort1(0, xs.length - 1)
  }

  val inputList = List(1, 4, 7, 3, 2, 9)
  println(s"Input List : ${inputList}")
  println(s"Sorted List : ${sortFunctionalWay(inputList)}")

  def sortFunctionalWay(xs: List[Int]): List[Int] = {
    if(xs.length <=1) xs
    else{
      val pivot = xs(xs.length/2)
      List.concat(sortFunctionalWay(xs.filter(pivot >)), xs.filter(pivot ==), sortFunctionalWay(xs.filter(pivot <)))
    }
  }








}
