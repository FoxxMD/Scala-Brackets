import BracketTree.Bracket.Base
import BracketTree.Node
import org.json4s.JObject
import org.json4s.JsonAST.JString
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

import scala.util.Random

/**
 * Created by Matthew on 12/2/2014.
 */
class BaseTest extends FlatSpec with Matchers with GivenWhenThen {


  "The base" should "should create a bracket with correct positions" in {

    Given("a 2v2 configuration of seats")
    val seatPos = List(4, 2, 6, 1, 3, 5, 7)
    val seats = for (pos <- seatPos) yield {
      Node(pos)
    }

    When("seats are added to the Bracket")
    val bracket = Base(seats)

    Then("getting the seats should return the original position list")
    //println(bracket.getSeats)
    //println(seats)
    assert(bracket.getSeats.equals(seats))
  }

  "The base" should "seed player data" in {

    Given("an empty Bracket")
    val seats = for (pos <- List(4, 2, 6, 1, 3, 5, 7)) yield Node(pos)
    val emptyBracket = Base(seats)


    And("a seed order")
    val seedOrder = List(1, 3, 5, 7)

    And("a list of seeded seats")
    val players = for (x <- seedOrder) yield randomPersonGen
    var playerCounter = 0
    val seededSeats = seats.map { x =>
      if (seedOrder.contains(x.position)) {
        val a = x.copy(payload = players.drop(playerCounter).headOption)
        playerCounter = playerCounter + 1
        a
      }
      else
        x
    } //seats.zipWithIndex.map { case (elem, index) => elem.copy(payload = players.lift(index)) }
    When("players are seeded to the empty bracket")
    val seededBracket = emptyBracket.seed(players, seedOrder)

    Then("the seeded bracket should equal the list of seededSeats")
    //println(seededBracket.getSeats)
    //println(seededSeats)
    assert(seededBracket.getSeats.equals(seededSeats))

  }

  "The bracket" should "be able to return a parent node based on position" in {

    val seats = for (pos <- List(8, 4, 12, 10, 14, 2, 6, 13, 15, 9, 11, 5, 7, 1, 3)) yield {
      Node(pos)
    }
    val bracket = Base(seats)
    assert(bracket.getParent(5).element.get.position == 6)
  }


  def randomPersonGen: JObject = {
    val names = List("charles", "michael", "lisa", "troy", "huey", "stan")
    val usernames = List("lee3", "jesus", "superman", "whatever", "iceman5", "lazers")
    JObject(List(("name", JString(Random.shuffle(names).head)), ("userName", JString(Random.shuffle(usernames).head))))
  }
}
