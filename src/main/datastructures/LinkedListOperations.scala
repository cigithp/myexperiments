package main.datastructures

/**
  * Created on 7/16/17.
  */
object LinkedListOperations {

  //linked list has 2 pointers
  //head
  //tail


  case class Node(data: Int, var next : Option[Node])
  //var head : Node

  object Node {
    def apply(data: Int) = new Node(data, None)
  }

  def findKey(key: Int, node: Option[Node]): Option[Node] = node match {
    case Some(x) if key == x.data => Some(x)
    case Some(x) => findKey(key, x.next)
    case _ => None
  }

  def traverseLL(node: Option[Node]): Option[Node] = node match {
    case Some(x) if x.next.isDefined => traverseLL(x.next)
    case Some(d) => Some(d)
    case _ => throw new NoSuchElementException
  }

  def insertLast(value: Int, head: Option[Node]): Node = {
    if (head.isEmpty) Node(value)
    else traverseLL(head) match {
      case Some(x) =>
        x.next = Some(Node(value))
        x
      case _ => throw new NoSuchElementException
    }
  }

//  def deleteFirst(key: Int, value: Int, head: Option[Node]): Option[Node] = {
//
//  }
//
//  def reverse




}
