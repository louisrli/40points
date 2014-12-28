object Suit extends Enumeration {
  type Suit = Value
  val Heart = Value("Heart")
  val Spade = Value("Spade")
  val Diamond = Value("Diamond")
  val Club = Value("Club")
  val Joker = Value("Joker")

  def getStandardSuits : ValueSet = 
    Suit.values - Joker

  def toAbbreviation(v : Value) : String = {
    v match {
      case Joker => Joker.toString  // don't abbreviate Jokers
      case _ => v.toString.charAt(0).toString
    }
  }
}

