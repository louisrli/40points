import scala.util.Random

/**
 * An immutable class representing a deck of playing cards
 *
 * Can be instantiated using a DeckFactory
 */
class Deck protected (cards: List[Card]) {
  // TODO(louisli): Consider a "protected deck" option where you can or can't print the 
  // contents of cards. Useful for debugging
  
  def shuffle : Deck = Deck.getCustomDeck(Random.shuffle(cards))
}

/**
 * Factory for creating Deck objects.
 */
object Deck {
  /**
   * Generate a shuffled 52-card standard playing card deck.
   */
  def getStandardDeck : Deck = new Deck(this.generateStandardCards) shuffle

  /**
   * Generate a shuffled 54-card deck -- the 52-card standard deck with a little joker and big joker.
   */
  def getStandardDeckJoker : Deck = new Deck(this.generateStandardCardsJokers) shuffle
  
  /**
   * Generate an unshuffled custom deck of cards from a list of cards.
   */
  def getCustomDeck(cards : List[Card]) = new Deck(cards)

  private def generateStandardCards : List[Card] = {
    for {
      rank <- Rank.getStandardRanks
      suit <- Rank.getStandardSuits
    } yield (Card(rank, suit))
  }

  private def generateStandardCardsJokers : List[Card] = {
    Card(Rank.LittleJoker, Suit.Joker) :: Card(Rank.BigJoker, Suit.Joker) :: this.generateStandardCards
  }
}
