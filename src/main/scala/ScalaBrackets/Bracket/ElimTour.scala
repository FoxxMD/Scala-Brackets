package ScalaBrackets.Bracket

import ScalaBrackets.Utility._
import ScalaBrackets._
import monocle.SimpleLens
import monocle.syntax._
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Extraction, Formats}

import scala.collection.immutable.SortedSet

/**
 * Created by Matthew on 11/24/2014.
 */

case class ElimTour(matches: SortedSet[Match], participants: Set[Participant] = Set(), seedOrder: Option[Array[Int]] = None) extends BaseTournament[ElimTour] {

  if (matches.count(x => x.inStartingRound) * 2 < participants.size)
    throw new IllegalArgumentException("Cannot create a tournament with more participants than starting seats")

  implicit val formats: Formats = DefaultFormats + new NoneJNullSerializer

  private[this] val _matches: SimpleLens[ElimTour, SortedSet[Match]] = SimpleLens[ElimTour](_.matches)((b, modifiedMatches) => b.copy(matches = modifiedMatches))
  private[this] val _homeSeat: SimpleLens[Match, Option[Seat]] = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _awaySeat = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _participants: SimpleLens[ElimTour, Set[Participant]] = SimpleLens[ElimTour](_.participants)((b, modifiedPars) => b.copy(participants = modifiedPars))


  def addParticipant(par: Participant): ElimTour = {
    if (matches.count(x => x.inStartingRound) * 2 == participants.size)
      throw new IllegalArgumentException("Cannot add more participants than starting seats")
    else {
      val zmatch = matches.find(x => x.inStartingRound && x.hasAvailableSeat).get
      this applyLens _matches modify (_.-(zmatch)) applyLens _matches modify (_.+(zmatch.addSeat(par.id))) applyLens _participants modify (_.+(par))
    }
  }

  def removeParticipant(parId: Int): ElimTour = {
    val participant = participants.find(x => x.id == parId).getOrElse(throw new Exception("No participant with the Id " + parId + " exists in this bracket."))

    if (getNonSeedMatches.flatMap(x => List(x.home, x.away)).flatten.exists(x => x.participantId == parId))
      throw new Exception("Cannot remove participant who has already progressed in bracket. Perhaps you want to change the participant payload?")

    val zmatch = getSeedMatches.find(x => x.includes(parId)).get

    this applyLens _participants modify (_.-(participant)) applyLens _matches modify (_.-(zmatch)) applyLens _matches modify (_.+(zmatch.removeSeat(parId)))
  }

  def updateParticipant(participant: Participant): ElimTour = {
    val oldParticipant = participants.find(x => x.id == participant.id).getOrElse(throw new Exception("No participant with the Id " + participant.id + "found."))
    this applyLens _participants modify (_.-(oldParticipant)) applyLens _participants modify (_.+(participant))
  }

  def seed(newParticipants: Option[Set[Participant]] = None, newSeedOrder: Option[Array[Int]] = None): ElimTour = {

    val usedParticipants = newParticipants.getOrElse(participants)
    val usedSeedOrder = newSeedOrder.getOrElse(seedOrder.getOrElse{
      usedParticipants.foldLeft(Array[Int]())((acc, elem) => acc :+ elem.id)
    })

    if (matches.count(x => x.inStartingRound) * 2 < usedParticipants.size)
      throw new Exception("Cannot create a tournament with more participants than starting seats")
    if (usedSeedOrder.size > 0 && usedSeedOrder.size < newParticipants.size)
      throw new Exception("Seed order list size does not correspond to participant list size")

    val (seedMatches, nonSeedMatches) = matches.partition(x => x.inStartingRound)
    var index = 0

    val newMatches = nonSeedMatches ++ seedMatches.foldLeft(SortedSet[Match]()) { (accum, element) =>
      index = index + 2
      accum + element.addSeat(usedParticipants.find(x => x.id == usedSeedOrder.drop(index - 2).head).get.id).addSeat(usedParticipants.find(x => x.id == usedSeedOrder.drop(index - 1).head).get.id)
    }

    this.copy(matches = newMatches)
  }

  def setScore(matchId: Int, homeScore: Option[Int] = None, awayScore: Option[Int] = None) = {
    val zmatch = matches.find(x => x.id == matchId).getOrElse(throw new Exception("Cannot find match with Id " + matchId))
    val newMatch = zmatch.copy(home = zmatch.home map (_.setScore(homeScore)))
    this applyLens _matches set matches.filterNot(x => x.id == zmatch.id).+(newMatch)
  }

  def advanceMatch(matchId: Int, winnerId: Int) = {
    val wonMatch = matches.find(_.id == matchId).getOrElse[Match](throw new Exception("No match with that Id found"))
    if (wonMatch.winnerTo.isDefined) {
      val advancingMatch = matches.find(_.id == wonMatch.winnerTo.get).get
      val first = updateMatches(matches, wonMatch, wonMatch.winner(winnerId))
      this.copy(matches = updateMatches(first, advancingMatch, advancingMatch.addSeat(wonMatch.winner.get.participantId)))
    }
    else
      this.copy(matches = updateMatches(matches, wonMatch, wonMatch.winner(winnerId)))

  }

  def getWinner: Option[Participant] = matches.last.winner.fold[Option[Participant]](None)(p => participants.find(x => x.id == p.participantId))

  def getMatchesByRound(round: Int) = matches.filter(x => x.round == round)

  def getMatch(matchId: Int) = matches.find(x => x.id == matchId).get

  private[this] def getNonSeedMatches: SortedSet[Match] = {
    matches.filter(x => !x.inStartingRound)
  }

  private[this] def getSeedMatches: SortedSet[Match] = {
    matches.filter(x => x.inStartingRound)
  }

  private[this] def updateMatches(zmatches: SortedSet[Match], oldMatch: Match, newMatch: Match): SortedSet[Match] = {
    matches.filterNot(_ == oldMatch) + newMatch
  }

  override def outputResultsJBracket: JValue = {

   val a = matches.foldLeft(Map[String, Map[String, List[List[Option[Int]]]]]()) { (acc, elem) =>
      //update bracket Map based on match bracket #
      acc updated(elem.bracket.toString, acc.get(elem.bracket.toString).fold {
        //if no bracket Map
        //create new round Map with new List(of matches)
        Map[String, List[List[Option[Int]]]]((elem.round.toString, List(matchToJBracketFormat(elem))))
      } {
        x =>
          //If bracket Map exists
          //update round Map based on match round #
          x updated(elem.round.toString, x.get(elem.round.toString).fold{
            //if no List of matches exists for this round
            //create new list of Matches
            List(matchToJBracketFormat(elem))
          }{
            //List of Match exists for this round, append new Match
            _ :+ matchToJBracketFormat(elem)
          })
      })
    }
    Extraction.decompose(a.values.map(x => x.values))
  }

  override def outputToJBracket = {
    render(("teams" -> outputParticipantsToJBracket) ~
      ("results" -> outputResultsJBracket))
  }

  def outputParticipantsToJBracket = {
    def a(elem: Match): Option[JValue] = elem.home.map(x => getParticipant(x.participantId).map(y => y.payload.getOrElse(render(y.id))))
    def b(elem: Match): Option[JValue] = elem.away.map(x => getParticipant(x.participantId).map(y => y.payload.getOrElse(render(y.id))))

    getSeedMatches.foldLeft(List(List[Option[JValue]]())){(acc, elem) => acc.+:(List(a(elem),b(elem)))  }
  }

  private[this] def getParticipant(id: Int) = { participants.find(x => x.id == id) }
  private[this] def matchToJBracketFormat(m: Match): List[Option[Int]] = {
    List(m.home.map(_.participantId),m.away.map(_.participantId))
  }
}
