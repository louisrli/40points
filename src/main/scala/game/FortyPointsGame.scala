package com.louis.fortypoints.game

import com.louis.fortypoints.card._

/**
 * An object for the progression of the forty points card game,
 * containing functions for the manipulation of the game state.
 */
object FortyPointsGame {
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
       state.nextTurn.copy(phase = HandDrawing)
      }
      case (HandSelectTrump, SetTrump(player, cards)) => {
        /* 1. [User input] let the user select a trump, otherwise continue */
       // Check if trump is already set
       // TODO(louisli) [multicard] let trump override
       // TODO need some type of indicator that it worked
       // TODO check that cards is equal to 1. if it's not, they should _be able to try again_
       // TODO validate that player is equal to current player
       state.trumpSuit match {
         case None if cards.size == 1 => 
           state.nextTurn.copy(trumpSuit = Some(cards.head.suit), phase = HandDrawing)
         case None if cards.size != 1 => state // let them try again
         case Some(suit) => state.nextTurn.copy(phase = HandDrawing)
       }
      }
      case (HouseBottomFilter, HouseFilterBottomCards(player, bottom)) => {
        /* 1. [User input] allow the house to swap bottom cards
         * with cards in his deck */
        // TODO(louisli)
        state.house match {
          case Some(house) => {
            require (house == player)  // TODO real error handling...ignore otherwise, log debug
            if (bottom.size == FortyPointsGame.numBottomCards) {
              // Check that all the cards are either in the bottom or in his hand
              val valid = bottom forall { 
                (c) => state.deck.cards.contains(c) && state.players(house).hand.contains(c) 
              }

              if (valid) {
                 // swap the cards
                 // TODO(louisli) where u left off
                 state
              }
              else {
                // return an error, asking only to specify valid cards
                state
              }
            } 
            else {
              state  // TODO(louisli) error message, let them try again
            }
          }
          case None =>  // TODO(louisli) This case should really never happen
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
   * Calculate the number of bottom cards from the number of players
   * and the number of decks.
   */
  private def numBottomCards: Int = {
    4  // TODO(louisli): Placeholder for now
  }
  private def numBottomCards(numPlayers: Int, numDecks: Int): Int = {
    4  // TODO(louisli)
  }
}
