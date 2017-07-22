package main.datastructures

/**
  * Created on 7/15/17.
  */
object ListOperations extends App{

  //a recursive solution
  def last[A](a : List[A]): A = a match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  val inputList = List(1, 4, 7, 3, 2, 9)
  println(s"Input List : ${inputList}")
  println(s"Last Element of the List : ${last(inputList)}")

  def penultimateElementNR[A](a : List[A]) : A = if (a.isEmpty) throw new NoSuchElementException else a.init.last

  println(s"Penultimate Element of the List : ${penultimateElementNR(inputList)}")

  //recursive
  def penultimateElement[A](a: List[A]): A = a match {
    case h :: List(t) => h
    case _ :: tail => penultimateElement(tail)
    case _ => throw new NoSuchElementException
  }

  println(s"Penultimate element of the List (recursive) : ${penultimateElement(inputList)}")

  //last nth element from a list
  def lastNth[A](n: Int, a : List[A]) : A =
    if (n <= 0)
      throw new IllegalArgumentException
    else if (n > a.length)
      throw new NoSuchElementException
    else a.takeRight(n).head

  println(s"Last Nth Element of the List : ${lastNth(3, inputList)}")

  //last nth element from a list - recursive
  def lastNthRecursive[A](n: Int, a: List[A]): A = a match {
    case tail if tail.length == n => tail.head
    case _ :: tail => lastNthRecursive(n, tail)
    case _ => throw new NoSuchElementException
  }

  println(s"Last Nth element of the List (recursive) : ${lastNthRecursive(3, inputList)}")

  //find kth element in a list
  def findKthNR[A](k: Int, a: List[A]): A = if(k >= 0 && k < a.length) a(k-1)  else throw new NoSuchElementException

  println(s"First Kth element of the List : ${findKthNR(4, inputList)}")

  //find kth element in a list - recursive#1
  def findKthRecursive[A](k: Int, a: List[A]): A = k match {
    case 1 => a.head
    case k if k > 0 => findKthRecursive(k-1, a.tail)
    case _ => throw new NoSuchElementException
  }

  def findKthRecursive2[A](k: Int, a: List[A]): A = (k,a) match {
    case (1, h::_) => h
    case (k, _::tail) => findKthRecursive2(k-1,tail)
    case _ => throw new NoSuchElementException
  }

  println(s"First Kth element of the List (recursive) : ${findKthRecursive2(4, inputList)}")

  //length of list - tail recursive solution
  def length[A](a: List[A]): Int = {
    def lengthN[A](n: Int, l: List[A]): Int = l match {
      case Nil => n
      case _::tail => lengthN(n+1, tail)
    }
    lengthN(0,a)
  }

  println(s"Length of the List (recursive) : ${length(inputList)}")

}
