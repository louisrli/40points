package com.louis.fortypoints.card

object Suit extends Enumeration {
  type Suit = Value
  val Heart = Value("Heart")
  val Spade = Value("Spade")
  val Diamond = Value("Diamond")
  val Club = Value("Club")
  val Joker = Value("Joker")

  def getStandardSuits : ValueSet = 
    Suit.values - Joker

  /**
   * Takes the first letter of a suit ("h", "s", ...) 
   * and matches it to the corresponding suit (Heart, Spade, ...)
   *
   */
  def fromAbbreviation(c: Char) : Suit.Value = {
    val uppercase : Char = Character.toUpperCase(c)
    val suit : Option[Suit.Value] = Suit.values find (_.toString.head == uppercase)
    suit match {
      case Some(s) => s
      case None => 
        throw new IllegalArgumentException(
          "Invalid abbreviation for suit: " + c.toString)
    }
  }

  def toAbbreviation(v : Value) : String = {
    v match {
      case Joker => Joker.toString  // don't abbreviate Jokers
      case _ => v.toString.charAt(0).toString
    }
  }
}

