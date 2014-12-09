package ScalaBrackets

import ScalaBrackets.Bracket.ElimTour

import scala.collection.immutable.SortedSet

/**
 * Created by Matthew on 12/9/2014.
 */

object SingleElimination {
  def generate4: ElimTour = {
    val matches = SortedSet(
      Match(id = 1, winnerTo = Option(3), round = 1),
      Match(id = 2, winnerTo = Option(3), round = 1),
      Match(id = 3, winnerTo = None, round = 2)
    )
    ElimTour(matches)
  }

  def generate8 : ElimTour = {
    val matches = SortedSet(
      Match(id = 1, winnerTo = Option(5), round = 1),
      Match(id = 2, winnerTo = Option(5), round = 1),
      Match(id = 3, winnerTo = Option(6), round = 1),
      Match(id = 4, winnerTo = Option(6), round = 1),
      Match(id = 5, winnerTo = Option(7), round = 2),
      Match(id = 6, winnerTo = Option(7), round = 2),
      Match(id = 7, winnerTo = None, round = 3)
    )
    ElimTour(matches)
  }
}
