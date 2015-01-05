package com.louis.fortypoints.game.console

import com.louis.fortypoints.game._
/**
 * Utility functions for the console version of the game.
 */
object ConsoleUtil {
  /**
   * Get the instructions displayed before user input on a given phase
   */
  def getPrePhaseDisplay(state: GameState): String = {
    val prettyHand: String = state.currentPlayer.hand map (_.toString) mkString ";"
    val currentHand = "Your current hand (%d): %s".format(state.currentTurn, prettyHand)
    state.phase match {
      case HouseSelection => "[UNIMPLEMENTED] Automatically selecting the house for now..." // TODO(louisli)
      case HandDrawing => "Drawing a card..."
      case HandSelectTrump => currentHand
      case HouseBottomFilter => "Bottom cards: " + state.deck.cards + "\n" + currentHand
      case HouseCallCards => "[UNIMPLEMENTED] Skipping house call cards for now..." // TODO(louisli)
      case RoundFirstTurn | RoundOtherTurn => currentHand
      case RoundEnd => "Ending the round" // TODO(louisli) to print the winner, this should really be after
      case CountPoints => "[UNIMPLEMENTED] Count points"
      case GameEnd => "Game end"
    }
  }
}
