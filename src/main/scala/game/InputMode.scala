package com.louis.fortypoints.game

/**
 * Case class representing different game modes in a given game phase.
 * RequestInput: the phase requires user input.
 * NoInput: the phase should continue on its own.
 */
sealed trait InputMode
case class RequestInput(prompt: String) extends InputMode
case object NoInput extends InputMode

object InputMode {
  /**
   * Map game phases to game mode based on whether user input is needd
   * at the beginning of the game mode
   * @return an [[InputMode]]
   */
  def getInputMode: GamePhase => (InputMode) = {
      case HouseSelection => NoInput
      case HandDrawing => NoInput
      case HandSelectTrump => NoInput // RequestInput("Set the trump (optional)")
      // TODO(louisli) change back
      case HouseBottomFilter => RequestInput("Select the cards to put on the bottom")
      case RoundFirstTurn | RoundOtherTurn => RequestInput("Select cards to play")
      case RoundEnd | CountPoints | GameEnd | GameQuit => NoInput
  }
}
