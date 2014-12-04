package ScalaBrackets.Bracket

import ScalaBrackets._
import monocle.SimpleLens
import monocle.syntax._

import scala.collection.immutable.SortedSet

/**
 * Created by Matthew on 11/24/2014.
 */

case class ElimTour(matches: SortedSet[Match], participants: Set[Participant] = None, seedOrder: Array[Int] = List()) extends BaseTournament[ElimTour] {

  if(matches.count(x => x.inStartingRound)*2 < participants.size)
    throw new IllegalArgumentException("Cannot create a tournament with more participants than starting seats")

  private[this] val _matches: SimpleLens[ElimTour, SortedSet[Match]] = SimpleLens[ElimTour](_.matches)((b, modifiedMatches) => b.copy(matches = modifiedMatches))
  private[this] val _homeSeat: SimpleLens[Match, Option[Seat]] = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _awaySeat = SimpleLens[Match](_.home)((m, s) => m.copy(home = s))
  private[this] val _participants: SimpleLens[ElimTour, Set[Participant]] = SimpleLens[ElimTour](_.participants)((b, modifiedPars) => b.copy(participants = modifiedPars))


  def addParticipant(par: Participant): ElimTour = {
    if(matches.count(x => x.inStartingRound && !x.hasAvailableSeat)*2 == participants.size)
      throw new IllegalArgumentException("Cannot add more participants than starting seats")
    else
    {
      val zmatch = matches.find(x => x.inStartingRound && x.hasAvailableSeat).get
      this applyLens _matches modify (_.-(zmatch)) applyLens _matches modify (_.+(zmatch.addSeat(par.id))) applyLens _participants modify(_.+(par))
    }
  }

  def removeParticipant(parId: Int): ElimTour = {
    val participant = participants.find(x => x.id == parId).getOrElse(throw new Exception("No participant with the Id " + parId + " exists in this bracket."))

    if(getNonSeedMatches.flatMap(x => List(x.home,x.away)).flatten.exists(x => x.participantId == parId))
      throw new Exception("Cannot remove participant who has already progressed in bracket. Perhaps you want to change the participant payload?")

    val zmatch = getSeedMatches.find(x => x.includes(parId)).get

    this applyLens _participants modify(_.-(participant)) applyLens _matches modify (_.-(zmatch)) applyLens _matches modify (_.+(zmatch.removeSeat(parId)))
  }

  def updateParticipant(participant: Participant): ElimTour = {
    val oldParticipant = participants.find(x => x.id == participant.id).getOrElse(throw new Exception("No participant with the Id " + participant.id + "found."))
    this applyLens _participants modify (_.-(oldParticipant)) applyLens _participants modify(_.+(participant))
  }

  def seed(newParticipants: Option[Set[Participant]] = None, newSeedOrder: Option[Array[Int]] = None): ElimTour ={

    val usedSeedOrder = newSeedOrder.getOrElse(seedOrder)
    val usedParticipants = newParticipants.getOrElse(participants)

    if(matches.count(x => x.inStartingRound)*2 < usedParticipants.size)
      throw new Exception("Cannot create a tournament with more participants than starting seats")
    if(usedSeedOrder.size > 0 && usedSeedOrder.size < newParticipants.size)
      throw new Exception("Seed order list size does not correspond to participant list size")

    val (seedMatches, nonSeedMatches) = matches.partition(x => x.inStartingRound)
    var index = 0

    val newMatches = nonSeedMatches ++ seedMatches.foldLeft(SortedSet[Match]()){ (accum, element) =>
      index = index + 2
      accum + element.addSeat(usedParticipants.find(x => x.id == usedSeedOrder.drop(index-2).head).get.id).addSeat(usedParticipants.find(x => x.id == usedSeedOrder.drop(index-1).head).get.id)
      }

    this.copy(matches = newMatches)
  }

  def setScore(matchId: Int, homeScore: Option[Int], awayScore: Option[Int]) = {
    val zmatch = matches.find(x => x.id == matchId).getOrElse(throw new Exception("Cannot find match with Id " + matchId))
    val newMatch = zmatch.copy(home = zmatch.home map (_.setScore(homeScore)))
    this applyLens _matches modify(_.-(zmatch)) applyLens _matches modify (_.+(newMatch))
  }

  def advanceMatch(matchId: Int, winnerId: Int) = {
    val wonMatch = matches.find(_.id == matchId).getOrElse[Match](throw new Exception("No match with that Id found"))
    if(wonMatch.winnerTo.isDefined)
    {
      val advancingMatch = matches.find(_.id == wonMatch.winnerTo.get).get
      val first = updateMatches(matches, wonMatch, wonMatch.winner(winnerId))
      this.copy(matches = updateMatches(first, advancingMatch,advancingMatch.addSeat(wonMatch.winner.get.participantId)))
    }
    else
     this.copy(updateMatches(matches, wonMatch, wonMatch.winner(winnerId)))

  }

  def getWinner: Option[Participant] = matches.last.winner.fold[Option[Participant]](None)(p => participants.find(x => x.id == p.participantId))

  def getMatchesByRound(round: Int) = matches.filter(x => x.round == round)

  private[this] def getNonSeedMatches: SortedSet[Match] = {
    matches.filter(x => !x.inStartingRound)
  }
  private[this] def getSeedMatches: SortedSet[Match] = {
    matches.filter(x => x.inStartingRound)
  }
  private[this] def updateMatches(zmatches: SortedSet[Match], oldMatch: Match, newMatch: Match): SortedSet[Match] = {
    matches.filterNot(_ == oldMatch) + newMatch
  }
}
