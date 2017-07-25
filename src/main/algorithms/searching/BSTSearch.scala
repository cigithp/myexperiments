package main.algorithms.searching

import main.datastructures.BSTOperations._
import _root_.main.datastructures.BSTOperations

/**
  * Created on 7/24/17.
  */
object BSTSearch extends App {

  def search(key: Int, root: Option[Node]): Boolean = root match {
    case Some(x) =>
      key match {
        case x if root.get.data == x => true
        case x if root.get.data < x => search(key, root.get.right)
        case x if root.get.data > x => search(key, root.get.left)
        case _ => false
      }
    case _ => false
  }
  val root = Node(10, None, None)

  BSTOperations.addNode(4, Some(root))
  BSTOperations.addNode(7, Some(root))
  BSTOperations.addNode(2, Some(root))
  BSTOperations.addNode(1, Some(root))
  BSTOperations.addNode(8, Some(root))
  BSTOperations.addNode(5, Some(root))
  BSTOperations.addNode(3, Some(root))

  println("Search 3 : "+search(3, Some(root)))
  println("Search 2 : "+search(2, Some(root)))
  println("Search 10 : "+search(10, Some(root)))
  println("Search 30 : "+search(30, Some(root)))
  println("Search 19 : "+search(19, Some(root)))


}
