package BracketTree

import org.json4s.JsonAST.JObject

/**
 * Created by Matthew on 11/24/2014.
 */
case class Node(position: Int, payload: Option[JObject],var left: Option[Node] = None,var right: Option[Node] = None) extends Ordered[Node] {
  override def compare(that: Node): Int = this.position - that.position
}
