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

  def this() = {
    this(List(), List(), None)
  }

  def this(hand: Hand) = {
    this(hand, List(), None)
  }

  /**
   * Returns a new Player with an added card in the hand
   */
  def addHandCard(card: Card): Player = {
    this.copy(hand = card:: this.hand)
  }

  /**
   * Returns a new player with a removed card from the hand
   *
   * If the hand does not contain the card, it does nothing
   */
  def removeHandCards(cards: List[Card]): Player = {
    this.copy(hand = this.hand diff cards)
  }

  /**
   * Returns a new Player with the added point cards
   *
   * Throws an exception if a card is not a point card.
   */
  def addPointCards(cards: List[Card]): Player = {
    require(cards forall (PointUtil.isPoint(_)))
    this.copy(points = cards ++ this.points)
  }

  def setPlay(p: Play): Player = {
    this.copy(currentPlay = Some(p))
  }

  /**
   * Sums the total points for a player
   */
  def tallyPoints: Int = {
    this.points.foldLeft(0)(_ + PointUtil.pointValue(_))
  }
}
