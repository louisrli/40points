package com.louis.fortypoints.game

import com.louis.fortypoints.card.Card
import com.louis.fortypoints.game.play.Play

/**
 * A class representing a player
 * @param hand
 *  The player's current hand
 * @param points
 *  The player's current accumulated point cards
 * @param currentPlay
 *  The current cards played by the player. None if the player
 *  has not yet played in the round.
 */
case class Player(
  hand: Hand, 
  points: List[Card],
  currentPlay: Option[Play]) {

  /**
   * Returns a new Player with an added card in the hand
   */
  def addHandCard(card : Card) : Player = {
    this.copy(hand = card :: this.hand)
  }

  /**
   * Returns a new Player with the added point card
   *
   * Throws an exception if the card parameter is not a point card.
   */
  def addPointCard(card : Card) : Player = {
    require(PointUtil.isPoint(card))
    this.copy(points = card :: this.points)
  }

  /**
   * Sums the total points for a player
   */
  def tallyPoints : Int = {
    this.points.foldLeft(0)(_ + PointUtil.pointValue(_))
  }


}
