package com.louis.fortypoints.game.play

import com.louis.fortypoints.card._

/**
 * The Play class represents a group of cards played by a player on their turn
 */

case class Play(
  cards: List[Card],
  rank: PlayRank) {

  def this(cards: List[Card]) = {
    this(cards, PlayUtil.determineRank(cards)) 
  }

  def this(cards: Card*) = {
    this(cards.toList, PlayUtil.determineRank(cards.toList))
  }

  override def toString: String = {
    "(" + (cards mkString ";") + ")"
  }
}

object Play {
  def fromAbbrevs(abbrevs: String*): Play = {
    new Play(abbrevs.toList map { Card(_) })
  }
}
