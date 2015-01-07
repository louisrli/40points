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
    // Reset error state
    val state = s.copy(error = CommandNoError)
    (state.phase, cmd) match {
      /* Misc */
      case (_, ExitCommand) => cmd.exec(state)
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
          state.copy(phase = HouseBottomFilter, currentTurn = state.house.get)
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
      case (HouseBottomFilter, HouseFilterBottomCards(_, _)) =>
        /* 1. [User input] allow the house to swap bottom cards
         * with cards in his deck */
        cmd.exec(state)
      case (HouseCallCards, _) =>
        /* 1. [User input] house can select cards to call */
       /* TODO[Unimplemented] until multideck */
        state
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
        require(state.getPlays.size == state.players.size)  // TODO(legit check)
        // TODO: should check that all people have the same number of cards
        val winningPlay = pu.determineWinner(state.getPlays flatMap { (s) => s } toList)
        val winner = state.getPlaysFlat.indexOf(winningPlay)
        // TODO should check that this is never -1
        if (state.currentPlayer.hand.size > 0)
          state.clearPlays.copy(currentTurn = winner, phase = RoundFirstTurn)
        else
          state.copy(phase = CountPoints)
      }
      /* Epilogue */
      case (CountPoints, _) =>
        val oppPoints = PointUtil.tallyTeamPoints(state.teamOpp)
        state.copy(
          houseWon = Some(oppPoints > state.pointThreshold),
          phase = GameEnd)
      case (GameEnd, _) =>
        /* IDK. Notify people that we done. */
        state
      case (_, BlankCommand) => state
      case (_, _) => ???
    }
  }


  /**
   * For commands that deal with the current player, check if the player
   * sending the command is the current player.
   *
   * If it's the wrong player, we just ignore it (this shouldn't be
   * allowed client-side).
   *
   * TODO: Log a warning.
   */
  private def check(p: Int, s: GameState) = p == s.currentTurn
  private def checkPlayer(state: GameState): Command => Boolean = {
    case SetTrump(p, c) => check(p, state)
    case HouseFilterBottomCards(p, c) => check(p, state)
    case MakePlay(p, c) => check(p, state)
    case _ => true
  }
}
