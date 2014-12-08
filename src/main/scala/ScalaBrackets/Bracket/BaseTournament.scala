package ScalaBrackets.Bracket

import ScalaBrackets._
import org.json4s.JsonAST.JValue

import scala.collection.immutable.SortedSet

/**
 * Created by Matthew on 12/3/2014.
 */
trait BaseTournament[T] {

  def matches: SortedSet[Match]
  def participants: Set[Participant]
  def seedOrder: Option[Array[Int]]

  def seed(newParticipants: Option[Set[Participant]], newSeedOrder: Option[Array[Int]]): BaseTournament[T]
  def addParticipant(par: Participant): BaseTournament[T]
  def removeParticipant(parId: Int): BaseTournament[T]
  def updateParticipant(participant: Participant): BaseTournament[T]
  def setScore(matchId: Int, homeScore: Option[Int], awayScore: Option[Int]): BaseTournament[T]
  def advanceMatch(matchId: Int, winnerId: Int): BaseTournament[T]

  def getWinner: Option[Participant]
  def getMatchesByRound(round: Int): SortedSet[Match]
  def getMatch(matchId: Int): Match

  def outputResultsJBracket: JValue
}
