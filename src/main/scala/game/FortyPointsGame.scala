package com.louis.fortypoints.game

import com.louis.fortypoints.card._
import com.louis.fortypoints.game.command._
import com.louis.fortypoints.game.play._

/**
 * An object for the progression of the forty points card game,
 * containing functions for the manipulation of the game state.
 */
object FortyPointsGame {
  // TODO(louisli): Refactor this to use the COMMAND PATTERN
  // without matching on command
  def update(s: GameState, cmd: Command) : GameState = {
    // Reset error state and winner
    val state = s.copy(error = CommandNoError, roundWinner = None)
    (state.phase, cmd) match {
      /* Misc */
      case (_, ExitCommand) => cmd.exec(state)
      /* Prologue */
      case (HouseSelection, _) => {
        // TODO(louisli): implement a method for house selection
        state.copy(house = 0, phase = HandDrawing, 
          teamHouse = List(0, 1))
      }
      case (HandDrawing, _) => {
        /* 1. Check if no more cards need to be drawn
         * 2. Otherwise, let the current player draw a card 
         * 3. While drawing, players can play trumps */
        if (state.deck.size == state.numBottomCards)
          state.copy(phase = HouseBottomFilter, currentTurn = state.house)
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
       state.nextTurn.copy(phase = HandDrawing)
      }
      case (HandSelectTrump, SetTrump(_, _)) =>
        /* 1. [User input] let the user select a trump, otherwise continue */
       cmd.exec(state)
      case (HouseBottomFilter, HouseFilterBottomCards(_, _)) => {
        /* 1. [User input] allow the house to swap bottom cards
         * with cards in his deck */
        val pu = new PlayUtil(state.trumpRank, state.trumpSuit.get)
        val filtered = cmd.exec(state)

        // Sort each player's hand
        filtered.copy(players = filtered.players map { p =>
          p.copy(hand = pu.sort(p.hand))
        })
      }
      /* Rounds */
      case (RoundFirstTurn, MakePlay(_, _)) =>
        /* 1. [User input] player makes a play
         * 2. Check if the card is a called card
         * 3. Set the rules on what subsequent cards can be played */
        cmd.exec(state)
      case (RoundOtherTurn, MakePlay(_, _)) =>
        /* 1. [User input] player makes a play
         * 2. Validate play _based on the user's hand_ 
         *  (e.g. if he has a pair, he must play it)
         * 3. Check if the card is a called card */
        cmd.exec(state)
      case (RoundEnd, _) => {
        /* 1. Gather the players plays and determine the winner
         * 2. Give any point cards to the winning player
         * 3. Clear hands */
        val pu = new PlayUtil(state.trumpRank, state.trumpSuit.get)
        val winningPlay = pu.determineWinner(state.getPlays flatMap { (s) => s } toList)
        val winner = state.getPlaysFlat.indexOf(winningPlay)

        // Defensively perform checks to make sure we're not in inconsistent state
        if (winner == - 1)
          state.copy(error = CommandInvalidConfig("Round end -- couldn't find the winning play"))
        else if (state.getPlays.size != state.players.size) {
          state.copy(error = CommandInvalidConfig(
            "Round end -- number of plays != number of players"))
        }
        else if (!state.players.forall { _.hand.size == state.currentPlayer.hand.size }) {
          state.copy(error = CommandInvalidConfig(
            "Round end -- not all players had the same number of cards"))
        }
        else if (state.currentPlayer.hand.size > 0) {
          // Start another round
          collectPoints(state, winner).clearPlays.copy(
            currentTurn = winner, 
            roundWinner = Some(winner, winningPlay), 
            phase = RoundFirstTurn)
        }
        else
          state.copy(phase = CountPoints, roundWinner = Some(winner, winningPlay))
      }
      /* Epilogue */
      case (CountPoints, _) =>
        val oppPoints = PointUtil.tallyTeamPoints(state.teamOppPlayers)
        state.copy(
          houseWon = Some((oppPoints > state.pointThreshold, oppPoints)),
          phase = GameEnd)
      case (GameEnd, _) =>
        /* IDK. Notify people that we done. */
        state.copy(phase = GameQuit)
      case (_, BlankCommand) => state
      case (_, _) => state  // TODO log a warning or something
    }
  }

  /**
   * Give all point cards in a round to the specified player
   */
  private def collectPoints(s: GameState, p: Int): GameState = {
    val points = s.getPlays flatMap (_.get.cards) filter PointUtil.isPoint
    s.updatePlayer(p, s.players(p).addPointCards(points.toList))
  }
}
