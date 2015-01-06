package com.louis.fortypoints.game

import com.louis.fortypoints.card._
import com.louis.fortypoints.game.command._
import com.louis.fortypoints.game.play._

/**
 * Command abstracts a user input (it could come from the console, the web, etc.)
 *
 * Only certain game phases require commands to continue.
 */
sealed trait Command {
  def exec(state: GameState): GameState
}

case class SetTrump(player: Int, cards: List[Card]) extends Command {
  override def exec(state: GameState): GameState = {
    state.trumpSuit match {
      case None if cards.size == 1 => 
        if (cards.head.rank == state.trumpRank)
          state.nextTurn.copy(trumpSuit = Some(cards.head.suit), phase = HandDrawing)
        else
          state.copy(error = CommandTrumpWrongRank(cards.head.rank, state.trumpRank))
      case None if cards.size != 1 => 
        // TODO(multicard): support nonsingle cards
        state.copy(error = CommandTrumpWrongArity(cards.size, 1))
      case Some(suit) => 
        state.nextTurn.copy(error = CommandTrumpAlreadySet)
    }
  }
}

/**
 * Swap cards from the player's hands with cards from the bottom.
 *
 * The player must be the house.
 */
case class HouseFilterBottomCards(player: Int, cards: List[Card]) extends Command {
  override def exec(state: GameState): GameState = {
    state.house match {
      case None =>  // This case should never really happen
        // TODO(louisli) log a warning?
        state.copy(error = CommandInvalidConfig, currentTurn = 0, phase = RoundFirstTurn)
      case Some(house) if house != player => 
        state.copy(error = CommandInvalidPlayer)
      case Some(house) if cards.size != FortyPointsGame.numBottomCards => 
        state.copy(error = CommandBottomWrongArity(cards.size, FortyPointsGame.numBottomCards))
      case Some(house) => {
        // Check that all the cards are either in the bottom or in his hand
        val validCards = cards filter { 
          (c) => state.deck.cards.contains(c) || state.currentPlayer.hand.contains(c) 
        }
        if (validCards.size == cards.size)
          swapBottom(state, house)
        else 
          state.copy(error = CommandBottomInvalidCards(cards diff validCards))
      } 
    }
  }

  /**
   * Given a list of bottom cards, put these bottom cards in the deck
   * and put any other cards in the player's hand
   */
  private def swapBottom(state: GameState, house: Int): GameState = {
    val bottomAndHand = state.deck.cards ++ state.players(house).hand
    val newBottom = this.cards
    val newHand = bottomAndHand diff newBottom
    val newPlayer = state.players(house).copy(hand = newHand)
    state
      .copy(deck = Deck.getCustomDeck(newBottom), phase = RoundFirstTurn)
      .updatePlayer(house, newPlayer)
  }

}

case class MakePlay(player: Int, cards: List[Card]) extends Command {
  override def exec(state: GameState): GameState = {
    // Check that the player actually has the input cards
    val hasCards = cards forall { state.currentPlayer.hand contains _ }
    if (player != state.currentTurn)
      state.copy(error = CommandInvalidPlayer)
    else if (!hasCards) {
      state.copy(error = CommandPlayInvalidCards(cards diff state.currentPlayer.hand))
    }
    else if (!state.isRoundStarted)
      execFirstPlay(state)
    else
      execOtherPlay(state)
  }

  /**
   * Handle validating and executing the first play of a round
   */
  private def execFirstPlay(state: GameState): GameState = {
    // TODO(louisli) instantiating a playUtil is too hard...
    state.trumpSuit match {
      case Some(suit) => {
        val pu = PlayUtil(state.trumpRank, suit)
        val play = new Play(cards)
        pu.validateFirstPlay(play) match {
          case None => state.setPlay(player, play)
          case Some(pve) => state.copy(error = CommandPlayError(pve))
        }
        state.copy(firstPlayer = state.currentTurn)
      }
      case None => state.copy(error = CommandInvalidConfig) 
    }
  }

  private def execOtherPlay(state: GameState): GameState = {
    state.trumpSuit match {
      case Some(suit) => {
        val pu = PlayUtil(state.trumpRank, suit)
        val play = new Play(cards)
        pu.validateOtherPlay(play, state.currentPlayer.hand, state.getPlaysOrdered.head) match {
          case None => state.setPlay(player, play)
          case Some(pve) => state.copy(error = CommandPlayError(pve))
        }
      }
      case None => state.copy(error = CommandInvalidConfig) 
    }
  }
}

case object ExitCommand extends Command {
  override def exec(state: GameState): GameState = sys.exit
}

case object BlankCommand extends Command {
  override def exec(state: GameState): GameState = state
}
