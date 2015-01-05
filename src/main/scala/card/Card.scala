package com.louis.fortypoints.card

class Card(val rank: Rank.Value, val suit: Suit.Value) {

  // TODO(louisli): throw an exception or somehow disallow Joker + facecard
  def isSpade: Boolean = (suit == Suit.Spade)
  def isDiamond: Boolean = (suit == Suit.Diamond)
  def isHeart: Boolean = (suit == Suit.Heart)
  def isClub: Boolean = (suit == Suit.Club)
  def isJoker: Boolean = (suit == Suit.Joker) || (rank == Rank.LittleJoker) ||
    (rank == Rank.BigJoker) 

  def isRed: Boolean = (isHeart || isDiamond)
  def isBlack: Boolean = (isSpade || isClub)


  override def toString : String = {
    if (suit == Suit.Joker)
      rank.toString
    else
      rank.toString + Suit.toAbbreviation(suit).toLowerCase
  }

  override def equals(other: Any) : Boolean = {
    other match {
      case other: Card => other.rank == this.rank && other.suit == this.suit
      case _ => false
    }
  }
}

object Card {
  final val BigJoker = new Card(Rank.BigJoker, Suit.Joker)
  final val LittleJoker = new Card(Rank.LittleJoker, Suit.Joker)

  def apply(rank: Rank.Value, suit: Suit.Value): Card = new Card(rank, suit)

  /**
   * Converts a string abbreviation "Kh" to the corresponding card
   */
  def apply(s: String): Card = {
    val littleJoker = Rank.LittleJoker.toString
    val bigJoker = Rank.BigJoker.toString
    s match {
      case `littleJoker` => new Card(Rank.LittleJoker, Suit.Joker)
      case `bigJoker` => new Card(Rank.BigJoker, Suit.Joker)
      case _ =>
        require(s.length == 2, "Instantiating card from an invalid abbreviation: " + s)
        new Card(Rank.withName(s(0).toString), Suit.fromAbbreviation(s(1)))

    }
  }


}
