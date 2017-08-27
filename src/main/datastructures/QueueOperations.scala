package main.datastructures

/**
  * Created on 7/16/17.
  */
object QueueOperations extends App {
  //LIST IMPLEMENTATION
  var queue : List[Int] = List.empty[Int]

  def apply(q: List[Int]) = {
    queue = q
  }

  def enqueue(elem: Int): List[Int] = {
    queue = queue :+ elem
    queue
  }

  def dequeue: List[Int] = queue match {
    case List() => throw new Exception("DEQUEUE ON AN EMPTY QUEUE")
    case x :: xs =>
      println(s"ELEMENT REMOVED :: $x")
      queue = xs
      queue
    case _ => throw new NoSuchElementException("ACTION NOT SUPPORTED")
   }

  //ARRAY IMPLEMENTATION
  var head: Int = -1
  var tail: Int = -1
  var queueArray: Array[Int] = new Array[Int](10)

  def apply(q: Array[Int]) = {
    queueArray = q
  }

  /**
    *
    * @param elem
    *
    * Conditions to check :
    * QUEUE OVERFLOW
    * For the First element - initialize HEAD and TAIL to 0
    * TAIL is pointing at the end and HEAD is pointing not at the first element - which means array still has space to enqueue
    * Append element at the end - the general condition
    */
  def enqueue2(elem: Int) : Unit = (head, tail) match {
    case (x,y) if x == 0 && y == queueArray.length - 1 => throw new Exception("QUEUE IS FULL")
    case (x,y) if y == queueArray.length - 1 =>
      tail = 0
      queueArray(tail) = elem
      display(queueArray)
    case (x,y) if x == -1 && y == -1 =>
      head = head + 1
      tail = tail + 1
      queueArray(tail) = elem
      display(queueArray)
    case _ =>
      tail = tail + 1
      queueArray(tail) = elem
      display(queueArray)
   }

  /**
    * Conditions to check :
    * QUEUE UNDERFLOW
    * For only one element in the QUEUE - reset HEAD and TAIL to -1
    * Increment HEAD for dequeue - the general condition
    */

  def dequeue2 : Unit = (head,tail) match {
    case (x,y) if x == -1 && y == -1 => throw new Exception("QUEUE IS EMPTY")
    case (x,y) if x == 0 && y == 0 =>
      queueArray(head) = 0
      head = -1
      tail = -1
    case _ =>
      queueArray(head) = 0
      head = head + 1
      display(queueArray)
  }

  def display(q: Array[Int]): Unit = {
    println("QUEUE :: ")
    for(i <- 0 until q.length)
      print(s"${q(i)}")
    println()
  }

  var array : Array[Int] = new Array[Int](10)
  QueueOperations(array)
  println(s"ENQUEUE :: ${enqueue2(7)}")
  println(s"ENQUEUE :: ${enqueue2(8)}")
  println(s"ENQUEUE :: ${enqueue2(9)}")
  println(s"ENQUEUE :: ${enqueue2(10)}")
  println(s"ENQUEUE :: ${enqueue2(11)}")
  println(s"ENQUEUE :: ${enqueue2(12)}")
  println(s"ENQUEUE :: ${enqueue2(13)}")
  println(s"ENQUEUE :: ${enqueue2(14)}")
  println(s"ENQUEUE :: ${enqueue2(15)}")
  println(s"DEQUEUE 1:: $dequeue2")
  println(s"ENQUEUE :: ${enqueue2(16)}")
  println(s"DEQUEUE 1:: ${dequeue2}")
  println(s"DEQUEUE 1:: ${dequeue2}")
  println(s"DEQUEUE 1:: ${dequeue2}")
  println(s"ENQUEUE :: ${enqueue2(17)}")
  println(s"ENQUEUE :: ${enqueue2(18)}")
  println(s"DEQUEUE 1:: ${dequeue2}")

//  println(s"STACK :: ${QueueOperations(List(1,2,3))}")
//  println(s"ENQUEUE :: ${enqueue(7)}")
//  println(s"DEQUEUE 1:: ${dequeue}")
//  println(s"DEQUEUE 2:: ${dequeue}")


}
