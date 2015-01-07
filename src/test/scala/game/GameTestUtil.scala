package com.louis.fortypoints.game

import com.louis.fortypoints.card._
import com.louis.fortypoints.game._
import com.louis.fortypoints.game.play._
import com.louis.fortypoints.game.command._

/**
 * Utility functions for helping test games
 */
object GameTestUtil {

  /**
   * Default game state for testing
   */
  val blankState: GameState = {
    GameState(
      players = Vector(new Player(), new Player(), new Player(), new Player()), 
      pointThreshold = 10,
      trumpSuit = None,
      trumpRank = Rank.Two,
      house = 0,
      deck = Deck.getStandardDeckJoker,
      firstPlayer = 0,
      currentTurn = 0,
      phase = HouseSelection,
      pendingCalledCards = List(),
      teamHouse = List(),
      teamOpp = List(),
      houseWon = None,
      error = CommandNoError)
  }

  /**
   * Game state where all the players have been dealt cards
   */
  val dealtState: GameState = {
    val (hands, rest) = Deck.getStandardDeckJoker.dealHands(12, 4)
    blankState.copy(
      players = (hands map { new Player(_) }).to[Vector],
      deck = rest)
  }

  /**
   * Return an instance of Play (when we don't care about the actual play)
   */
  val getPlay: Play = {
    new Play(Card("Ah"))
  }

}
