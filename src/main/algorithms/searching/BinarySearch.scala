package main.algorithms.searching

/**
  * Created by pillaci on 7/16/17.
  *
  * Assumption : Input array is already sorted
  */
object BinarySearch extends App {


  def binarySearch(list: Array[Int], searchValue: Int): Boolean = {
    def bsf(list: Array[Int], searchValue: Int, start: Int, end: Int) : Boolean = {
      if(start > end) return false
      val mid = start + (end - start + 1)/2
      list match {
        case (xs: Array[Int]) if xs(mid) == searchValue => return true
        case (xs: Array[Int]) if xs(mid) > searchValue => bsf(list, searchValue, start, mid - 1)
        case (xs: Array[Int]) if xs(mid) < searchValue => bsf(list, searchValue, mid + 1, end)
      }
    }
    bsf(list, searchValue, 0, list.length - 1)
  }



  def binarySearchRecursive(list: Array[Int], searchValue: Int)(start: Int, end: Int) : Boolean = {
    if(start > end) return false
    val mid = start + (end - start + 1)/2
    if (list(mid) == searchValue) return true
    else if (list(mid) > searchValue) binarySearchRecursive(inputList, 11)(start, mid - 1)
    else binarySearchRecursive(inputList, 11)(mid + 1, end)
  }

  val inputList = Array(1,2,3,4,5,6,7,8,9,10)
  println(s"Input List : $inputList")
  println(s"Search :: ${binarySearch(inputList, 5)}")
  val x = binarySearchRecursive(inputList, 11)_
  println(s"Search :: ${x(0, inputList.length - 1)}")
}
