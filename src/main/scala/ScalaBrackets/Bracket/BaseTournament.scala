package ScalaBrackets.Bracket

import ScalaBrackets._
import org.json4s.JsonAST.JValue

/**
 * Created by Matthew on 12/3/2014.
 */
trait BaseTournament[T] {

  def matches: List[Match]
  def participants: Set[Participant]
  def seedOrder: Seq[Int]
  def id: String

  def seed(newParticipants: Option[Set[Participant]], newSeedOrder: Option[Seq[Int]]): BaseTournament[T]
  def addParticipant(par: Participant): BaseTournament[T]
  def removeParticipant(parId: Int): BaseTournament[T]
  def updateParticipant(parId: Int, participant: Participant): BaseTournament[T]
  def setScore(matchId: Int, homeScore: Option[Int], awayScore: Option[Int]): BaseTournament[T]
  def advanceMatch(matchId: Int, winnerId: Int): BaseTournament[T]

  def getWinner: Option[Participant]
  def getMatchesByRound(round: Int): List[Match]
  def getMatch(matchId: Int): Match

  def outputResultsJBracket: JValue
  def outputParticipantsToJBracket: JValue
  def outputToJBracket: JValue

}
