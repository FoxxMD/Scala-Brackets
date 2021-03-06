package ScalaBrackets

/**
 * Created by Matthew on 12/3/2014.
 */
case class Seat(participantId: Int, score: Option[Int] = None, winner: Option[Boolean] = None) {

    def setScore(s: Option[Int]) = {
      this.copy(score = s)
    }
}
