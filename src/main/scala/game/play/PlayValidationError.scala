package com.louis.fortypoints.game.play

import com.louis.fortypoints.card.Suit

sealed trait PlayValidationError
case class SizeError(expected: Int, actual: Int) extends PlayValidationError
case class HasTrumpsError() extends PlayValidationError  // TODO(louisli) refactor as case object
case class HasSuitError(suit: Suit.Value) extends PlayValidationError
case object InvalidPlayError extends PlayValidationError

object PlayValidationError {
  private def handContainsMsg(s: String): String = {
    val sl = s.toLowerCase
    "This is a %s round, and your hand contains a %s. You must play a %s"
      .format(sl, sl, sl)
  }

  def getMessage: PlayValidationError => String = {
    case SizeError(expected, actual) =>
      "Please select %d cards (you selected %d)".format(actual, expected)
    case HasTrumpsError() => handContainsMsg("trump")
    case HasSuitError(suit) => handContainsMsg(suit.toString)
    case InvalidPlayError => "The cards you selected don't form a valid hand."
  }
}
