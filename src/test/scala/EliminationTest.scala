import ScalaBrackets.Bracket.ElimTour
import ScalaBrackets.{Match, Participant}
import org.json4s.JsonAST.{JObject, JString}
import org.json4s.jackson.JsonMethods._
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

import scala.util.Random

/**
 * Created by Matthew on 12/2/2014.
 */
class EliminationTest extends FlatSpec with Matchers with GivenWhenThen {

  behavior of "an elimination tournament with BaseTournament trait"

  it should "initialize given only a set of matches" in {
    ElimTour(matches = sampleSingleElimMatches)
  }
  it should "add a participant" in {
    val tour = ElimTour(matches = sampleSingleElimMatches)

    val participant = Participant(1,payload = Option(randomPersonGen))

    val tourWithP = tour.addParticipant(participant)

    assert(tourWithP.participants.size.equals(1))
    assert(tourWithP.participants.head.equals(participant))
  }
  it should "remove a participant" in {
    val tour = ElimTour(matches = sampleSingleElimMatches)

    val participant = Participant(1,payload = Option(randomPersonGen))

    val tourWithP = tour.addParticipant(participant)

    assert(tourWithP.removeParticipant(1).participants.size.equals(0))
  }
  it should "update a participant" in {
    val tour = ElimTour(matches = sampleSingleElimMatches)

    val participant = Participant(1,payload = Option(randomPersonGen))

    val tourWithP = tour.addParticipant(participant)

    val updatedParticipant = Participant(1, payload = Option(randomPersonGen))

    assert(tourWithP.updateParticipant(1, updatedParticipant).participants.head.equals(updatedParticipant))
  }
  it should "seed starting matches" in {
    val players = for (x: Int <- 1 to 8) yield {Participant(id = x, payload = Option(randomPersonGen))}

    Given("no seed order")
    val tour = ElimTour(matches = sampleSingleElimMatches, participants = players.toSet).seed()

    Then("seeds are random-ish")
    assert(tour.getMatchesByRound(1).map(x => List(x.home.get.participantId, x.away.get.participantId)).flatten.size == 8)

    Given("a seed order")
    val seedOrder = players.foldRight(Array[Int]())((elem, acc) => acc :+ elem.id)
    Then("seed order is equivalent")
    val seededTour = ElimTour(matches = sampleSingleElimMatches, participants = players.toSet).seed(newSeedOrder = Option(seedOrder))
    assert(seededTour.getMatchesByRound(1).foldLeft(Array[Int]())((accum, elem) => accum :+ elem.home.get.participantId :+ elem.away.get.participantId).deep == seedOrder.deep)
  }
  it should "set the score for a given match" in {
    val players = for (x: Int <- 1 to 8) yield {Participant(id = x, payload = Option(randomPersonGen))}
    val tour = ElimTour(matches = sampleSingleElimMatches, participants = players.toSet).seed()

    val scoredTour = tour.setScore(1, Option(2), Option(3)).setScore(2, Option(1))
    assert(scoredTour.getMatch(1).home.get.score.get == 2)
    assert(scoredTour.getMatch(2).away.get.score == None)
  }
  it should "advance a match" in {
    val players = for (x: Int <- 1 to 8) yield {Participant(id = x, payload = Option(randomPersonGen))}
    val tour = ElimTour(matches = sampleSingleElimMatches, participants = players.toSet).seed()

    val advancedTour = tour.advanceMatch(1, tour.getMatch(1).away.get.participantId)
    assert(advancedTour.getMatch(5).home.get.participantId == tour.getMatch(1).away.get.participantId)
  }

  val sampleSingleElimMatches = List(
    Match(id = 1, winnerTo = Option(5), round = 1),
    Match(id = 2, winnerTo = Option(5), round = 1),
    Match(id = 3, winnerTo = Option(6), round = 1),
    Match(id = 4, winnerTo = Option(6), round = 1),
    Match(id = 5, winnerTo = Option(7), round = 2),
    Match(id = 6, winnerTo = Option(7), round = 2),
    Match(id = 7, winnerTo = None, round = 3)
  )

  def randomPersonGen: JObject = {
    val names = List("charles", "michael", "lisa", "troy", "huey", "stan")
    val usernames = List("lee3", "jesus", "superman", "whatever", "iceman5", "lazers")
    JObject(List(("name", JString(Random.shuffle(names).head)), ("userName", JString(Random.shuffle(usernames).head))))
  }

  behavior of "an elimination tournament for JSON"

  it should "output JSON for jquery Brackets" in {
    val players = for (x: Int <- 0 to 7) yield {Participant(id = x, payload = Option(randomPersonGen))}
    val tour = ElimTour(matches = sampleSingleElimMatches, participants = players.toSet).seed()

    val advancedTour = tour.advanceMatch(1, tour.getMatch(1).away.get.participantId)

    println(pretty(render(advancedTour.outputToJBracket)))
  }

}
