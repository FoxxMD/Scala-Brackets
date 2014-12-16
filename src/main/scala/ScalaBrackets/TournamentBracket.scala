package ScalaBrackets

import ScalaBrackets.Bracket.ElimTour

/**
 * Created by Matthew on 12/9/2014.
 */

object SingleElimination {

  def generate2: ElimTour = {
    val matches = List(
      Match(id = 1, winnerTo = None, round = 1)
    )
    ElimTour(matches)
  }

  def generate4: ElimTour = {
    val matches = List(
      Match(id = 1, winnerTo = Option(3), round = 1),
      Match(id = 2, winnerTo = Option(3), round = 1),
      Match(id = 3, winnerTo = None, round = 2)
    )
    ElimTour(matches)
  }

  def generate8 : ElimTour = {
    val matches = List(
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
  def generate16: ElimTour = {
    val matches = List(
      Match(id = 1, winnerTo = Option(9), round = 1),
      Match(id = 2, winnerTo = Option(9), round = 1),
      Match(id = 3, winnerTo = Option(10), round = 1),
      Match(id = 4, winnerTo = Option(10), round = 1),
      Match(id = 5, winnerTo = Option(11), round = 1),
      Match(id = 6, winnerTo = Option(11), round = 1),
      Match(id = 7, winnerTo = Option(12), round = 1),
      Match(id = 8, winnerTo = Option(12), round = 1),
      Match(id = 9, winnerTo = Option(13), round = 2),
      Match(id = 10, winnerTo = Option(13), round = 2),
      Match(id = 11, winnerTo = Option(14), round = 2),
      Match(id = 12, winnerTo = Option(14), round = 2),
      Match(id = 13, winnerTo = Option(15), round = 3),
      Match(id = 14, winnerTo = Option(15), round = 3),
      Match(15, winnerTo = None, round = 4)
    )
    ElimTour(matches)
  }
}

object DoubleElimination {

  def generate4: ElimTour = {
    val matches = List(
      Match(id = 1, winnerTo = Option(3), loserTo = Option(4), round = 1),
      Match(id = 2, winnerTo = Option(3), loserTo = Option(4), round = 1),
      Match(id = 3, winnerTo = Option(5), round = 2),
      Match(id = 4, winnerTo = Option(5), round = 1, bracket = 2),
      Match(id = 5, winnerTo = Option(6), round = 2, bracket = 2),
      Match(id = 6, winnerTo = Option(8), round = 1, bracket = 3),
      Match(id = 7, winnerTo = None, round = 1, bracket = 3),
      Match(id = 8, winnerTo = None, round = 2, bracket = 3)
    )
    ElimTour(matches)
  }

  def generate8 : ElimTour = {
    val matches = List(
      Match(id = 1, winnerTo = Option(5), loserTo = Option(8), round = 1),
      Match(id = 2, winnerTo = Option(5), loserTo = Option(8), round = 1),
      Match(id = 3, winnerTo = Option(6), loserTo = Option(9), round = 1),
      Match(id = 4, winnerTo = Option(6), loserTo = Option(9), round = 1),
      Match(id = 5, winnerTo = Option(7), loserTo = Option(11), round = 2),
      Match(id = 6, winnerTo = Option(7), loserTo = Option(10), round = 2),
      Match(id = 7, winnerTo = Option(14), loserTo = Option(13), round = 3),
      Match(id = 8, winnerTo = Option(10), round = 1, bracket = 2),
      Match(id = 9, winnerTo = Option(11), round = 1, bracket = 2),
      Match(id = 10, winnerTo = Option(12), round = 2, bracket = 2),
      Match(id = 11, winnerTo = Option(12), round = 2, bracket = 2),
      Match(id = 12, winnerTo = Option(13), loserTo = Option(15), round = 3, bracket = 2),
      Match(id = 13, winnerTo = Option(14), loserTo = Option(15), round = 4, bracket = 2),
      Match(id = 14, winnerTo = Option(16), round = 1, bracket = 3),
      Match(id = 15, winnerTo = None, round = 1, bracket = 3),
      Match(id = 16, winnerTo = None, round = 2, bracket = 3)
    )
    ElimTour(matches)
  }
}