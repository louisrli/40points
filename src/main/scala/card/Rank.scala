object Rank extends Enumeration {
  type Rank = Value
  val Ace = Value(1, "A")
  val Two = Value(2, "2")
  val Three = Value(3, "3")
  val Four = Value(4, "4")
  val Five = Value(5, "5")
  val Six = Value(6, "6")
  val Seven = Value(7, "7")
  val Eight = Value(8, "8")
  val Nine = Value(9, "9")
  val Ten = Value(10, "T")
  val Jack = Value(11, "J")
  val Queen = Value(12, "Q")
  val King = Value(13, "K")
  val LittleJoker = Value(14, "Little Joker")
  val BigJoker = Value(15, "Big Joker")

  def getStandardRanks : ValueSet = "TODO(louisli)"
}

