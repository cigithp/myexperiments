package main.datastructures

/**
  * Created on 7/24/17.
  */
object BSTOperations extends App {

  case class Node(data: Int, var left: Option[Node], var right: Option[Node])

  def inOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
    inOrder(x.left)
    println(x.data)
    inOrder(x.right)
    case _ =>
  }

  def postOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
      postOrder(x.left)
      postOrder(x.right)
      println(x.data)
  }

  def preOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
      println(x.data)
      preOrder(x.left)
      preOrder(x.right)
    case _ =>
  }

  def addNode(key: Int, root: Option[Node]): Unit = root match {
    case Some(x) =>
      key match {
        case h if h < x.data && x.left.isEmpty => x.left = Some(Node(h, None, None))
        case h if h > x.data && x.right.isEmpty => x.right = Some(Node(h, None, None))
        case h if h < x.data && x.left.isDefined => addNode(h, x.left)
        case h if h > x.data && x.right.isDefined => addNode(h, x.right)
      }
    case _ =>
  }



}
