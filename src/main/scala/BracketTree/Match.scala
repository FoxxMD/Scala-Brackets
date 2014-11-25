package BracketTree

/**
 * Created by Matthew on 11/24/2014.
 */
case class Match(seats: Seq[Node], winnerTo: Option[Int], loserTo: Option[Int]) {

  def includes(seat: Node): Boolean = {
    seats.contains(seat)
  }
}
