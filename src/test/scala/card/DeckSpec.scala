package com.louis.fortypoints.card

import org.scalatest._
import com.louis.fortypoints.UnitSpec

class DeckSpec extends UnitSpec {
  "A Deck" should "shuffle immutably" in {
    val deck = Deck.getStandardDeck
    val shuffled = deck.shuffle

    // Check that a new object is returned via equality
    deck should not equal (shuffled)
    deck.size should equal (shuffled.size)
  }

  it should "deal correctly when dealing fewer cards than whole deck" in {
    val deck = Deck.getStandardDeck
    val (hands, rest) = deck.dealHands(12, 4)
    hands.size should equal (4)
    hands.flatten.size should equal (48)
    hands.head.size should equal (12)
    rest.size should equal (52 - 48)
  }

  it should "deal correctly when dealing same number of cards as whole deck" in {
    val deck = Deck.getStandardDeck
    val (hands, rest) = deck.dealHands(4, 13)
    hands.size should equal (13)
    hands.flatten.size should equal (52)
    hands.head.size should equal (4)
    rest.size should equal (0)
  }

  it should "allow for drawing the top card of a deck" in {
    val deck = Deck.getStandardDeck
    val (card, rest) = deck.draw
    rest.size should be (deck.size - 1)
  }

  it should "throw an exception when drawing from an empty deck" in {
    val deck = Deck.getEmptyDeck
    a [DeckException] should be thrownBy deck.draw
  }

  it should "throw an exception when trying to deal more cards than the deck" in {
    val deck = Deck.getStandardDeck
    a [DeckException] should be thrownBy deck.dealHands(5, 13)
  }

  "A Deck Factory" should "create an empty deck" in {
    val deck = Deck.getEmptyDeck
    deck.size should be (0)
    deck should be ('empty)
  }

  it should "create a custom deck" in {
    val deck = Deck.getCustomDeck (List(
      Card(Rank.Ace, Suit.Spade), Card(Rank.King, Suit.Diamond)
    ))
    deck.size should be (2)
  }

  it should "create a standard deck" in {
    val deck = Deck.getStandardDeck
    deck.size should be (52)
    deck should not be ('empty)
  }

  it should "create a standard deck with jokers" in {
    val deck = Deck.getStandardDeckJoker
    deck.size should be (54)
    deck should not be ('empty)
  }
}

