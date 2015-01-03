package com.louis.fortypoints.card

import scala.util.Random

/**
 * An immutable class representing a deck of playing cards
 *
 * Can be instantiated with a DeckFactory
 */
class Deck protected (cards: List[Card]) {
  // TODO(louisli): Consider a "protected deck" option where you can or can't print the 
  // contents of cards. Useful for debugging
  
  /**
   * Returns a shuffled version of this deck -- same cards, different order.
   */
  def shuffle : Deck = Deck.getCustomDeck(Random.shuffle(cards))

  /**
   * Draw a card from the top of the deck and return the card and the remaining deck.
   */
  def draw : (Card, Deck) = {
    if (!this.isEmpty)
      (this.cards.head, Deck.getCustomDeck(this.cards.tail))
    else
      throw new DeckException("Tried to draw from an empty deck")
  }

  /**
   * Divide the deck into numHands hands of size handSize, shuffling the deck first
   * Returns the hands and the remaining deck 
   *
   * Throws an exception if the number of cards requested is more than
   * the number of cards in the deck
   */
  def dealHands(handSize: Int, numHands: Int) : (List[List[Card]], Deck) = {
    val shuffled = Random.shuffle(cards)
    val numDealt = handSize * numHands
    val hands = shuffled.grouped(handSize).toList
    if (numDealt == this.size)
      (hands, Deck.getEmptyDeck)
    else if (numDealt < this.size)
      (hands.take(numHands), 
       Deck.getCustomDeck(hands takeRight(hands.size - numHands) flatten))
    else
      throw new DeckException("Tried to deal more cards than there were in the deck.")
  }

  /**
   * Returns the number of cards in the deck
   */
  def size : Int = cards.size

  /**
   * Returns true if there are no cards in the deck
   */
  def isEmpty : Boolean = cards.isEmpty
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
   * Generate an empty deck
   */
  def getEmptyDeck : Deck = new Deck(List())

  /**
   * Generate an unshuffled custom deck of cards from a list of cards.
   */
  def getCustomDeck(cards : List[Card]) : Deck = new Deck(cards)

  private def generateStandardCards : List[Card] = {
    (for {
      rank <- Rank.getStandardRanks
      suit <- Suit.getStandardSuits
    } yield (Card(rank, suit))) toList
  }

  private def generateStandardCardsJokers : List[Card] = {
    Card(Rank.LittleJoker, Suit.Joker) :: Card(Rank.BigJoker, Suit.Joker) :: this.generateStandardCards
  }
}
