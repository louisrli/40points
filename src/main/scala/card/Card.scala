package com.louis.fortypoints.card

class Card(val rank: Rank.Value, val suit: Suit.Value) {

  // TODO(louisli): throw an exception or somehow disallow Joker + facecard
  def isSpade = (suit == Suit.Spade)
  def isDiamond = (suit == Suit.Diamond)
  def isHeart = (suit == Suit.Heart)
  def isClub = (suit == Suit.Club)
  def isJoker = (suit == Suit.Joker) || (rank == Rank.LittleJoker) || 
    (rank == Rank.BigJoker) 

  def isRed = (isHeart || isDiamond)
  def isBlack = (isSpade || isClub)


  override def toString = rank.toString + Suit.toAbbreviation(suit).toLowerCase

  override def equals(other: Any) : Boolean = {
    other match {
      case other: Card => other.rank == this.rank && other.suit == this.suit
      case _ => false
    }
  }
}

object Card {
  def apply(rank: Rank.Value, suit: Suit.Value) = new Card(rank, suit)
}
