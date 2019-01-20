package exampleapp.services

import exampleapp.services.DmService.{DmTree, Node, Value}

object DmService {

  sealed trait DmTree

  case class Value(value: String) extends DmTree

  case class Node(children: Map[String, DmTree]) extends DmTree

}

class DmService {

  var dmTree: Node = Node(Map.empty)

  setValue(List("Device", "Details", "Manufacturer"), "asus")
  setValue(List("Device", "Details", "Model"), "n551jm")
  setValue(List("Device", "Details", "Version"), "3.0.1")

  def getDm: Node = {
    dmTree
  }

  def setValue(path: List[String], value: String): Unit = {
    require(path.nonEmpty)
    dmTree = replace(dmTree, path, Some(value))
  }

  def removeValue(path: List[String]): Unit = {
    require(path.nonEmpty)
    dmTree = replace(dmTree, path, None)
  }

  private def replace(tree: Node, p: List[String], newVal: Option[String]): Node = {
    p match {
      case head :: Nil =>
        val newMap: Map[String, DmTree] = newVal match {
          case Some(v) => tree.children + (head -> Value(v))
          case None => tree.children - head
        }
        Node(newMap)
      case head :: tail =>
        val currentSubNode = tree.children.get(head).collect { case n: Node => n }.getOrElse(Node(Map.empty))
        val newSubNode = replace(currentSubNode, tail, newVal)
        Node(tree.children + (head -> newSubNode))
      case _ => throw new IllegalArgumentException("empty path")
    }
  }
}
