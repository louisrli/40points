package com.louis.fortypoints.game

import com.louis.fortypoints.card._

/**
 *
 */
// TODO rename
object GameLoop {

  // TODO(louisli): Refactor this to use the COMMAND PATTERN
  // without matching on command
  def update(state: GameState, cmd: Command) : GameState = {
    (state.phase, cmd) match {
      /* Misc */
      case (_, ExitCommand) => sys.exit  // TODO(louisli): eventually replace with some graceful quit
      /* Prologue */
      case (HouseSelection, _) => {
        // TODO(louisli): implement a method for house selection
        state.copy(house = Some(0), phase = HandDrawing)
      }
      case (HandDrawing, _) => {
        /* 1. Check if no more cards need to be drawn
         * 2. Otherwise, let the current player draw a card 
         * 3. While drawing, players can play trumps (TODO) */
        val NumBottomCards = 4 // TODO(louisli): calculate the correct number of bottom cards
        if (state.deck.size == NumBottomCards)
          state.copy(phase = HouseBottomFilter)
        else {
          // Draw a card for the current player, then update the game phase
          val (card, rest) = state.deck.draw
          val newPlayer = state.currentPlayer.addHandCard(card)
          state
            .updatePlayer(state.currentTurn, newPlayer)
            .copy(deck = rest, phase = HandSelectTrump)
        }
      }
      case (HandSelectTrump, BlankCommand) => {
        /* 1. [User input] let the user select a trump, otherwise continue */
       state.nextTurn.copy(phase = HandDrawing)
      }
      case (HouseBottomFilter, _) => {
        /* 1. [User input] allow the house to swap bottom cards
         * with cards in his deck */
        // TODO(louisli)
        // For now, just pretend they can't swap bottom cards
        state.house match {
          case Some(house) =>
            // TODO(louisli) implement swapping routine
            state.copy(currentTurn = house, phase = RoundFirstTurn)
          case None =>
            state.copy(currentTurn = 0, phase = RoundFirstTurn)
        }
      }
      case (HouseCallCards, _) =>
        /* 1. [User input] house can select cards to call */
        state
      /* Rounds */
      case (RoundFirstTurn, _) =>
        /* 1. [User input] player makes a play
         * 2. Check if the card is a called card
         * 3. Set the rules on what subsequent cards can be played */
        state
      case (RoundOtherTurn, _) =>
        /* 1. [User input] player makes a play
         * 2. Validate play _based on the user's hand_ 
         *  (e.g. if he has a pair, he must play it)
         * 3. Check if the card is a called card */
        state
      case (RoundEnd, _) =>
        /* 1. Gather the players plays and determine the winner
         * 2. Give any point cards to the winning player
         * 3. Clear hands */
        state.clearPlays
      /* Epilogue */
      case (CountPoints, _) =>
        val oppPoints = PointUtil.tallyTeamPoints(state.teamOpp)
        state.copy(
          houseWon = Some(oppPoints > state.pointThreshold),
          phase = GameEnd)
      case (GameEnd, _) =>
        /* IDK. Notify people that we done. */
        state
      case (_, _) => ???
    }
  }

  /**
   * Case class representing different game modes
   * TODO(louisli): document
   * TODO(louisli): think of a better name?
   */
  sealed trait GameMode
  case class RequestInput(prompt: String) extends GameMode
  case object ContinueGame extends GameMode

  /**
   * Map game phases to game mode based on whether we need to request user
   * input at the beginning of the game mode
   * TODO(louisli): document
   */
  def getGameMode: GamePhase => (GameMode) = {
      case HouseSelection => ContinueGame
      case HandDrawing => ContinueGame
      case HandSelectTrump => RequestInput("Set the trump (optional)")
      case HouseBottomFilter => RequestInput("Select the cards to put on the bottom")
      case HouseCallCards => ContinueGame
      case RoundFirstTurn | RoundOtherTurn => RequestInput("Select cards to play")
      case RoundEnd => ContinueGame
      case CountPoints => ContinueGame
      case GameEnd => ContinueGame
  }

}
