package main.datastructures


/**
  * Created on 7/16/17.
  * LIFO strategy
  */
object StackOperations extends App {
  //LIST IMPLEMENTATION
  var stack: List[Int] = List.empty[Int]

  def apply(s: List[Int]) = {
    stack = s
  }

  //pre-pend the element in the stack
  def push(elem: Int): List[Int] = {
    stack = elem :: stack
    stack
  }

  //get the top element out and update the stack
  def pop : Int = stack match {
    case List() => throw new Exception("POP ON AN EMPTY STACK")
    case x :: xs =>
      stack = xs
      x
    case _ => throw new NoSuchElementException("ACTION NOT SUPPORTED")
  }

  //get the top element. no changes to the stack
  def peek: Int = stack match {
    case List() => throw new Exception("PEEK ON AN EMPTY STACK")
    case x :: xs => x
    case _ => throw new NoSuchElementException("ACTION NOT SUPPORTED")
  }

  //ARRAY IMPLEMENTATION

  var top: Int = -1
  var stackArray: Array[Int] = new Array[Int](10)

  def apply(s: Array[Int]) = {
    stackArray = s
  }

  /**
    *
    * @param elem
    *
    * Conditions to check :
    * STACK OVERFLOW
    * Increment TOP for adding value to stack - the general condition
    *
    */
  def push2(elem: Int): Unit = top match {
    case y if y == stackArray.length - 1 => throw new Exception("STACK OVERFLOW")
    case _ =>
      top = top + 1
      stackArray(top) = elem
      display(stackArray)
  }

  /**
    *
    * @return
    *
    * Conditions to check :
    * STACK UNDERFLOW
    * Decrement TOP to return the value and set the value to 0 - the general condition
    *
    */
  def pop2 : Int = top match {
    case -1 => throw new Exception("STACK UNDERFLOW")
    case _ =>
      val r = stackArray(top)
      stackArray(top) = 0
      top = top - 1
      display(stackArray)
      r
  }

  /**
    *
    * @return
    *
    * Return the value pointed by TOP
    *
    */
  def peek2 : Int = top match {
    case -1 => throw new Exception("STACK IS EMPTY")
    case _ => stackArray(top)
  }

  def display(s: Array[Int]): Unit = {
    println("STACK :: ")
    for(i <- 0 until s.length)
      print(s"${s(i)}")
    println("")
  }

  var array = new Array[Int](10)
  StackOperations(array)
  println(s"PUSH :: ${push2(7)}")
  println(s"PUSH :: ${push2(8)}")
  println(s"PUSH :: ${push2(9)}")
  println(s"PUSH :: ${push2(10)}")
  println(s"PUSH :: ${push2(11)}")
  println(s"PUSH :: ${push2(12)}")
  println(s"PEEK :: $peek2")
  println(s"POP :: $pop2")
  println(s"PUSH :: ${push2(13)}")
  println(s"POP :: $pop2")

  StackOperations(List())
  println(s"PUSH :: ${push(7)}")
  println(s"PUSH :: ${push(8)}")
  println(s"PUSH :: ${push(9)}")
  println(s"PUSH :: ${push(10)}")
  println(s"PUSH :: ${push(11)}")
  println(s"PUSH :: ${push(12)}")
  println(s"PEEK :: $peek")
  println(s"POP :: $pop")
  println(s"PUSH :: ${push(13)}")
  println(s"POP :: $pop")




}
