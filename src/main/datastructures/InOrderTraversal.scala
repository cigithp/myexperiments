package main.datastructures

import scala.collection.mutable.Stack

/**
  * Created on 8/25/17.
  */
object InOrderTraversal12 extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  def inOrderTraversal(root: TreeNode) : List[Int] = {
    var node : TreeNode = root
    val stack : Stack[TreeNode] = Stack[TreeNode]()
    val result : List[Int] = List.empty[Int]
    while(node != null || stack.nonEmpty) {
      while(node != null){
        stack.push(node)
        node = node.left
      }
      node = stack.pop
      result :+ node.value
      node = node.right
    }
    result
  }

  val root = new TreeNode(1)
  val node1 = new TreeNode(2)
  val node2 = new TreeNode(3)
  root.right = node1
  node1.left = node2

  println(inOrderTraversal(root))

}
