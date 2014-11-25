package BracketTree.Bracket

import BracketTree._
import org.json4s.JsonAST.JObject

/**
 * Created by Matthew on 11/24/2014.
 */
case class Base(seats: Array[Node], matches: Array[Match], seedOrder: Array[Int] = Array()) {

  private var tree = BST[Node]()

  for(x <- seats){ tree = tree.+(x)}

  def add(position: Int, payload: Option[JObject]) = {
    tree = tree.+(new Node(position,payload))
  }

  def replace(newNode: Node): Unit = {
    tree = tree.-(newNode)._2.+(newNode)
  }

  def seed(players: List[JObject]): Unit ={
    var playerCounter = 0
    if(tree.depthMap.head._2.size != players.size)
      throw new IllegalArgumentException("number of players and number of starting seats does not match")
    else if(seedOrder.size == 0){
     for(x <-tree.depthMap.head._2){
       tree = tree.-(x)._2.+(x.copy(payload = players.drop(playerCounter).headOption))
       playerCounter = playerCounter+1
     }
    }
    else{
      if(players.size != seedOrder.size)
        throw new IllegalArgumentException("number of players and number of seedOrder elements do not match")
      else if(seedOrder.size != 0 && seedOrder.size != tree.depthMap.head._2.size)
        throw new IllegalArgumentException("seedOrder does not correspond to the starting number of seats")
      else{
        for(x <- seedOrder){
          val replacingNode = tree.filter(y => y.position == x).toList.head
          tree = tree.-(replacingNode)._2.+(replacingNode.copy(payload = players.drop(playerCounter).headOption))
          playerCounter = playerCounter+1
        }
      }
    }
  }

  def winner: Node = tree.levelOrder(List[Node]())(_ :: _).reverse.head

  def at(pos: Int): Node = tree.filter(x => x.position == pos).toList.head
}
