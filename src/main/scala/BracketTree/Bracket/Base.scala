package BracketTree.Bracket

import BracketTree._
import org.json4s.JObject

/**
 * Created by Matthew on 11/24/2014.
 */

case class Base(seats: List[Node], matches: Array[Match] = Array(), seedOrder: List[Int] = List()) {

  private var tree = BST[Node]()

  for(x <- seats){ tree = tree.+(x)}

  def add(position: Int, payload: Option[JObject]) = {
    tree = tree.+(new Node(position,payload))
  }

  def replace(newNode: Node): Base = {
    //tree = tree.-(newNode)._2.+(newNode)
    tree = tree.map(x => if(x.position == newNode.position) newNode else x)
    this
  }

  def seed(players: List[JObject], newSeedOrder: List[Int] = List()): Base ={
    val usedSeedOrder = if(newSeedOrder.size != 0) newSeedOrder else seedOrder
    var playerCounter = 0
    if(tree.depthMap.last._2.size != players.size)
      throw new IllegalArgumentException("number of players and number of starting seats does not match")
    else if(usedSeedOrder.size == 0){
     for(x <-tree.depthMap.head._2){
       tree = tree.-(x)._2.+(x.copy(payload = players.drop(playerCounter).headOption))
       playerCounter = playerCounter+1
     }
    }
    else{
      if(players.size != usedSeedOrder.size)
        throw new IllegalArgumentException("number of players and number of seedOrder elements do not match")
      else if(usedSeedOrder.size != 0 && usedSeedOrder.size != tree.depthMap.last._2.size)
        throw new IllegalArgumentException("seedOrder does not correspond to the starting number of seats")
      else{
        for(x <- usedSeedOrder){
          val replacingNode = tree.filter(y => y.position == x).toList.head
          tree = tree.-(replacingNode)._2.+(replacingNode.copy(payload = players.drop(playerCounter).headOption))
          playerCounter = playerCounter+1
        }
      }
    }
    this
  }

  def winner: Node = tree.levelOrder(List[Node]())(_ :: _).reverse.head

  def at(pos: Int): Node = tree.filter(x => x.position == pos).toList.head

  def getSeats: List[Node] = tree.levelOrder(List[Node]())(_ :: _).reverse//tree.preOrder(List[Node]())(_ :: _).reverse//tree.levelOrder(List[Node]())(_ :: _).reverse.toArray

  def matchWinner(seat: Node) = {
      val matchParent = getParent(seat.position)
      ???
  }

  def getParent(seat: Int) = {
   tree.findParent(x => x.position == seat)
  }
}
