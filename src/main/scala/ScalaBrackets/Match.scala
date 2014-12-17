package ScalaBrackets

/**
 * Created by Matthew on 11/24/2014.
 */
case class Match(id: Int, home: Option[Seat] = None, away: Option[Seat] = None, winnerTo: Option[Int] = None, loserTo: Option[Int] = None, round: Int, bracket: Int = 1) extends Ordered[Match] {

  def includes(id: Int): Boolean = {
    (home.isDefined && home.get.participantId == id) || (away.isDefined && away.get.participantId == id)
  }

  def hasAvailableSeat: Boolean = {
    home.isEmpty || away.isEmpty
  }

  def inStartingRound: Boolean = {
    round == 1 && bracket == 1
  }

  def addSeat(participantId: Int): Match = {
    if(home.isEmpty)
      this.copy(home = Option(Seat(participantId)))
    else if(away.isEmpty)
      this.copy(away = Option(Seat(participantId)))
    else
      throw new Exception("Both seats are non-empty, cannot add participant to match.")
  }

  def removeSeat(participantId: Int): Match = {
    if(home.isDefined && home.get.participantId == participantId)
      this.copy(home = None)
    else if(away.isDefined && away.get.participantId == participantId)
      this.copy(away = None)
    else
      throw new Exception("Neither match seat contains the participantId " + participantId)
  }

  def updateSeat(oldParId: Int, newParId: Int): Match = {
    if(home.isDefined && home.get.participantId == oldParId)
      this.copy(home = Option(home.get.copy(participantId = newParId)))
    else if(away.isDefined && away.get.participantId == oldParId)
      this.copy(away = Option(away.get.copy(participantId = newParId)))
    else
      throw new Exception("Neither match seat contains the participantId " + oldParId)
  }

  def updateScore(participantId: Int, score: Int): Match = {
    if(home.isDefined && home.get.participantId == participantId) {
      this.copy(home = home map (_.setScore(Option(score))))
    } else if(away.isDefined && away.get.participantId == participantId){
      this.copy(away = away map (_.setScore(Option(score))))
    } else
      throw new Exception("Participant Id " + participantId + " not found in match " + id + " seats")
  }

  def hasScores: Boolean = {
    if(!hasParticipants)
      false
    else
      home.get.score.isDefined && away.get.score.isDefined
  }

  def getHighScorer: Int = {
    if(!hasScores)
      throw new Exception("Can't find the high scorer if both seats don't have scores!")
    if(home.get.score.get > away.get.score.get)
      home.get.participantId
    else
      away.get.participantId
  }

  def winner: Option[Seat] = {
    if(home.isEmpty && away.isEmpty)
      None
    else if(home.isDefined && home.get.winner.isDefined && home.get.winner.get)
      home
    else if(away.isDefined && away.get.winner.isDefined && away.get.winner.get)
      away
    else
      None
  }

  def winner(participantId: Int) = {
    if(!hasParticipant)
      throw new Exception("Empty match cannot have a winner.")

    if(home.isDefined && home.get.participantId == participantId)
      this.copy(home = Option(home.get.copy(winner = Option(true))), away = Option(away.get.copy(winner = Option(false))))
    else
      this.copy(away = Option(away.get.copy(winner = Option(true))), home = Option(home.get.copy(winner = Option(false))))
  }

  def loser: Option[Seat] = {
    val win = winner
    if(win.isEmpty)
      None
    else if (win.get == home.get)
      away
    else
      home
  }

  override def compare(that: Match): Int = this.id - that.id

  private[this] def hasParticipants: Boolean = {
    home.isDefined && away.isDefined
  }
  private[this] def hasParticipant: Boolean = {
    home.isDefined || away.isDefined
  }
}
