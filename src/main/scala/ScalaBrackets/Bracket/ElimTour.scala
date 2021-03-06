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
import com.novus.salat.annotations._

/**
 * Created by Matthew on 11/24/2014.
 */

case class ElimTour(matches: List[Match], participants: Set[Participant] = Set(), seedOrder: Seq[Int] = Seq.empty, @Key("_id") id: String = "") extends BaseTournament[ElimTour] {

  if (matches.count(x => x.inStartingRound) * 2 < participants.size)
    throw new IllegalArgumentException("Cannot create a tournament with more participants than starting seats")

  implicit val formats: Formats = DefaultFormats + new NoneJNullSerializer

  private[this] val _matches: SimpleLens[ElimTour, List[Match]] = SimpleLens[ElimTour](_.matches)((b, modifiedMatches) => b.copy(matches = modifiedMatches))
  private[this] val _homeSeat: SimpleLens[Match, Option[Seat]] = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _awaySeat = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _participants: SimpleLens[ElimTour, Set[Participant]] = SimpleLens[ElimTour](_.participants)((b, modifiedPars) => b.copy(participants = modifiedPars))


  def addParticipant(par: Participant): ElimTour = {
    if (matches.count(x => x.inStartingRound) * 2 == participants.size)
      throw new IllegalArgumentException("Cannot add more participants than starting seats")
    else {
      val zmatch = matches.find(x => x.inStartingRound && x.hasAvailableSeat).get
      this applyLens _matches modify (_ diff List(zmatch)) applyLens _matches modify (_.:+(zmatch.addSeat(par.id))) applyLens _participants modify (_.+(par))
    }
  }

  def removeParticipant(parId: Int): ElimTour = {
    val participant = participants.find(x => x.id == parId).getOrElse(throw new Exception("No participant with the Id " + parId + " exists in this bracket."))

    if (getNonSeedMatches.flatMap(x => List(x.home, x.away)).flatten.exists(x => x.participantId == parId))
      throw new BracketException("Cannot remove participant who has already progressed in bracket. Perhaps you want to change the participant payload?")

    val zmatch = getSeedMatches.find(x => x.includes(parId)).get

    this applyLens _participants modify (_.-(participant)) applyLens _matches modify (_ diff List(zmatch)) applyLens _matches modify (_.:+(zmatch.removeSeat(parId)))
  }

  def updateParticipant(parId: Int, participant: Participant): ElimTour = {
    val oldParticipant = participants.find(x => x.id == parId).getOrElse(throw new Exception("No participant with the Id " + parId + "found."))
    val updatedMatches = matches.map{x =>
      if(x.includes(parId))
        x.updateSeat(parId, participant.id)
      else
        x
    }
    this applyLens _participants modify (_.-(oldParticipant)) applyLens _participants modify (_.+(participant)) copy (matches = updatedMatches)
  }

  def seed(newParticipants: Option[Set[Participant]] = None, newSeedOrder: Option[Seq[Int]] = None): ElimTour = {

    val usedParticipants = newParticipants.getOrElse(participants)
    val usedSeedOrder = newSeedOrder.getOrElse(if (seedOrder.nonEmpty) seedOrder else {
      usedParticipants.foldLeft(Seq[Int]())((acc, elem) => acc :+ elem.id)
    })

    if (matches.count(x => x.inStartingRound) * 2 < usedParticipants.size)
      throw new Exception("Cannot create a tournament with more participants than starting seats")
    if (usedSeedOrder.size > 0 && usedSeedOrder.size < newParticipants.size)
      throw new Exception("Seed order list size does not correspond to participant list size")

    val (seedMatches, nonSeedMatches) = matches.sortBy(x => x.id).partition(x => x.inStartingRound)
    var index = 0

    val newMatches = nonSeedMatches ++ seedMatches.sortBy(x => x.id).foldLeft(SortedSet[Match]()) { (accum, element) =>
      var homeOption: Option[Participant] = None
      var awayOption: Option[Participant] = None
      if(usedSeedOrder.lift(index).isDefined)
        homeOption = usedParticipants.find(x => x.id == usedSeedOrder.lift(index).get).headOption
      if(usedSeedOrder.lift(index+1).isDefined)
        awayOption = usedParticipants.find(x => x.id == usedSeedOrder.lift(index+1).get).headOption

      if(homeOption.isDefined && awayOption.isEmpty)
      {
        index = index + 1
        accum + element.addSeat(homeOption.get.id)
      }
      else if(homeOption.isDefined && awayOption.isDefined)
      {
        index = index + 2
        accum + element.addSeat(homeOption.get.id).addSeat(awayOption.get.id)
      }
      else
        accum + element
    }

    this.copy(matches = newMatches, participants = usedParticipants)
  }

  def setScore(matchId: Int, homeScore: Option[Int] = None, awayScore: Option[Int] = None) = {
    val zmatch = matches.find(x => x.id == matchId).getOrElse(throw new Exception("Cannot find match with Id " + matchId))
    val newMatch = zmatch.copy(home = zmatch.home map (_.setScore(homeScore)), away = zmatch.away map (_.setScore(awayScore)))
    this applyLens _matches set matches.filterNot(x => x.id == zmatch.id).:+(newMatch)
  }

  def setScore(matchId: Int, participantId: Int, score: Int) = {
    val zmatch = matches.find(x => x.id == matchId).getOrElse[Match](throw new Exception("Cannot find match with Id " + matchId)).updateScore(participantId, score)
    val newMatch = if(zmatch.hasScores) {
      zmatch.winner(zmatch.getHighScorer)
    }else{
      zmatch
    }
    this applyLens _matches set matches.filterNot(x => x.id == zmatch.id).:+(newMatch)
  }

  def advanceMatch(matchId: Int, winnerId: Int): ElimTour = {
    val wonMatch = matches.find(_.id == matchId).getOrElse[Match](throw new Exception("No match with that Id found"))
    val tourWithUpdatedWinner = if (wonMatch.winnerTo.isDefined) {
      val advancingMatch = matches.find(_.id == wonMatch.winnerTo.get).get
      val first = updateMatches(matches, wonMatch, wonMatch.winner(winnerId))
      this.copy(matches = updateMatches(first, advancingMatch, advancingMatch.addSeat(winnerId)))
    }
    else
      this.copy(matches = updateMatches(matches, wonMatch, wonMatch.winner(winnerId)))
    val tourwithUpdatedLoser = if(wonMatch.loserTo.isDefined){
      val advancingLosingMatch = tourWithUpdatedWinner.matches.find(_.id == wonMatch.winnerTo.get).get
      tourWithUpdatedWinner.copy(matches = updateMatches(tourWithUpdatedWinner.matches, advancingLosingMatch, advancingLosingMatch.addSeat(wonMatch.loser.get.participantId)))
    }
    else
      tourWithUpdatedWinner
    //this will always be the most updated tournament. if there is no loser than tourWithUpdatedLoser = tourWithUpdatedWinner
    tourwithUpdatedLoser
  }

  def getWinner: Option[Participant] = matches.sortBy(x => x.id).last.winner.fold[Option[Participant]](None)(p => participants.find(x => x.id == p.participantId))

  def getMatchesByRound(round: Int) = matches.filter(x => x.round == round).sortBy(x => x.id)

  def getMatch(matchId: Int) = matches.find(x => x.id == matchId).get

  private[this] def getNonSeedMatches: List[Match] = {
    matches.filter(x => !x.inStartingRound).sortBy(x => x.id)
  }

  private[this] def getSeedMatches: List[Match] = {
    matches.filter(x => x.inStartingRound).sortBy(x => x.id)
  }

  private[this] def updateMatches(zmatches: List[Match], oldMatch: Match, newMatch: Match): List[Match] = {
    matches.filterNot(_ == oldMatch) :+ newMatch sortBy(x => x.id)
  }

  override def outputResultsJBracket: JValue = {

    val a = matches.sortBy(x => x.id).foldLeft(Map[String, Map[String, List[List[Option[String]]]]]()) { (acc, elem) =>
      //update bracket Map based on match bracket #
      acc updated(elem.bracket.toString, acc.get(elem.bracket.toString).fold {
        //if no bracket Map
        //create new round Map with new List(of matches)
        Map[String, List[List[Option[String]]]]((elem.round.toString, List(matchToJBracketFormat(elem))))
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
    def a(elem: Match): Option[JValue] = elem.home.map(x => getParticipant(x.participantId).map(y => render(("id" -> y.id) ~ y.payload.get)))
    def b(elem: Match): Option[JValue] = elem.away.map(x => getParticipant(x.participantId).map(y => render(("id" -> y.id) ~ y.payload.get)))

    val x = getSeedMatches.sortBy(x => x.id).foldLeft(List(List[Option[JValue]]())){(acc, elem) =>
      acc.:+(List(a(elem),b(elem)))
    }
    x.drop(1)
  }

  private[this] def getParticipant(id: Int) = { participants.find(x => x.id == id) }
  private[this] def matchToJBracketFormat(m: Match): List[Option[String]] = {
    List(seatToJBracketFormat(m.home),seatToJBracketFormat(m.away), Option(m.id.toString))
  }
  private[this] def seatToJBracketFormat(s: Option[Seat]): Option[String] = {
    s.fold[Option[String]](None){x => x.score.fold[Option[String]](x.winner.fold[Option[String]](None)(j => Option(j.toString))){k => Option(k.toString)}}
  }
}
