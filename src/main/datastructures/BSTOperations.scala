package main.datastructures

import main.datastructures.BSTOperations.Node

/**
  * Created on 7/24/17.
  */
object BSTOperations {

  case class Node(data: Int, var left: Option[Node], var right: Option[Node])

  def inOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
    inOrder(x.left)
    print(x.data+" || ")
    inOrder(x.right)
    case _ =>
  }

  def postOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
      postOrder(x.left)
      postOrder(x.right)
      print(x.data+" || ")
    case _ =>
  }

  def preOrder(root: Option[Node]): Unit = root match {
    case Some(x) =>
      print(x.data+" || ")
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

  def deleteNode(key: Int, root: Option[Node]): Node = {

    //traverse the tree to get the node
    //node can be : root, leaf, any other node
    //find out the successor for each case
    //replace the outgoing node with the successor

    var currNode: Option[Node] = root
    var parentNode: Option[Node] = None // this is wrong
    var isLeftChild: Boolean = false
    var rootNode : Node = root.get

    while (currNode.isDefined && key != currNode.get.data) {
      parentNode = currNode
      key match {
        case x if x < currNode.get.data =>
          currNode = currNode.get.left
          isLeftChild = true
        case x if x > currNode.get.data =>
          isLeftChild = false
          currNode = currNode.get.right
        case _ =>
      }
    }
    //for leaf
    if (currNode.isDefined && currNode.get.left.isEmpty && currNode.get.right.isEmpty)
      isLeftChild match {
        case true if parentNode.isDefined => parentNode.get.left = None
        case false if parentNode.isDefined => parentNode.get.right = None
        case _ => throw new NoSuchElementException
      }

    //for non-leaf with one right child
    if (currNode.isDefined && currNode.get.left.isEmpty && currNode.get.right.nonEmpty)
      isLeftChild match {
        case true if parentNode.isDefined => parentNode.get.left = currNode.get.right
        case false if parentNode.isDefined => parentNode.get.right = currNode.get.right
        case _ => throw new NoSuchElementException
      }

    //for non-leaf with one left child
    if (currNode.isDefined && currNode.get.left.isEmpty && currNode.get.left.nonEmpty)
      isLeftChild match {
        case true if parentNode.isDefined => parentNode.get.left = currNode.get.left
        case false if parentNode.isDefined => parentNode.get.right = currNode.get.left
        case _ => throw new NoSuchElementException
      }


    //for non-leaf with both children - can be root itself
    if (currNode.isDefined && currNode.get.left.nonEmpty && currNode.get.right.nonEmpty) {
      val successor = findSuccessor(currNode.get)
      parentNode match {
        case Some(x) if isLeftChild => x.left = Some(successor)
        case Some(x) => x.right = Some(successor)
        case _ => rootNode = successor
      }
    }
    rootNode
  }
  def findSuccessor(node: Node): Node = {
    var successorParent = node
    var successor = node
    var currentNode = node.right

    while(currentNode.isDefined){
      successorParent = successor
      successor = currentNode.get
      currentNode = currentNode.get.left
    }

    if(successor != node.right.get){
      successorParent.left = successor.right
      successor.right = node.right
    }
    successor.left = node.left
    successor
  }
}

object Main extends App {

  val root = Node(10, None, None)

  BSTOperations.addNode(4, Some(root))
  BSTOperations.addNode(7, Some(root))
  BSTOperations.addNode(2, Some(root))
  BSTOperations.addNode(1, Some(root))
  BSTOperations.addNode(8, Some(root))
  BSTOperations.addNode(5, Some(root))
  BSTOperations.addNode(13, Some(root))
  BSTOperations.addNode(12, Some(root))
  BSTOperations.addNode(15, Some(root))

  println(s"INORDER :: ${BSTOperations.inOrder(Some(root))}")
  //  println(s"PREORDER :: ${preOrder(Some(root))}")
  //  println(s"POSTORDER :: ${postOrder(Some(root))}")

  //delete leaf
   // println(s"INORDER :: ${BSTOperations.inOrder(Some(BSTOperations.deleteNode(5, Some(root))))}")

  //delete root
  //println(s"INORDER :: ${BSTOperations.inOrder(Some(BSTOperations.deleteNode(10, Some(root))))}")

  //delete non-leaf
  println(s"INORDER :: ${BSTOperations.inOrder(Some(BSTOperations.deleteNode(7, Some(root))))}")

}
