package com.louis.fortypoints.card

import org.scalatest._
import com.louis.fortypoints.UnitSpec

class CardSpec extends UnitSpec {
  private val spade = Card(Rank.Ace, Suit.Spade)
  private val heart = Card(Rank.Ace, Suit.Heart)
  private val diamond = Card(Rank.Ace, Suit.Diamond)
  private val club = Card(Rank.Ace, Suit.Club)
  private val joker = Card(Rank.BigJoker, Suit.Joker)
  private val littleJoker = Card(Rank.LittleJoker, Suit.Joker)

  "A Card" should "have helper functions to determine its suit" in {
    spade should be a 'spade
    diamond should be a 'diamond
    heart should be a 'heart
    club should be a 'club
  }

  it should "have helper functions to determine its color" in {
    spade should be ('black)
    club should be ('black)
    diamond should be ('red)
    heart should be ('red)
    spade should not be ('red)
    diamond should not be ('black)
  }

  it should "consider anything with rank or suit of joker to a joker" in { 
    joker should be ('joker)
    joker should not be ('red)
    joker should not be ('black)
    joker should not be a ('spade)
  }

  it should "be equal to another card of the same rank and suit" in {
    val a = Card(Rank.King, Suit.Spade)
    val b = Card(Rank.King, Suit.Spade)
    a should equal (b)
  }

  it should "not be equal to another card of different rank and suit" in {
    spade should not equal (diamond)
  }

  it should "allow instantiation from abbreviations" in {
    Card("Little Joker") should equal (littleJoker)
    Card("Big Joker") should equal (joker)
    Card("Kh") should equal (Card(Rank.King, Suit.Heart))
    Card("9c") should equal (Card(Rank.Nine, Suit.Club))
    an [IllegalArgumentException] should be thrownBy Card("foofoo")
  }

  "The Rank enum" should "give access to the thirteen standard ranks" in {
    val ranks = Rank.getStandardRanks
    ranks.size should equal (13)
    ranks should not contain (Rank.BigJoker)
    ranks should not contain (Rank.LittleJoker)
  }

  "The Suit enum" should "give access to the four standard suits" in {
    val suits = Suit.getStandardSuits
    suits.size should equal (4)
    suits should not contain (Suit.Joker)
  }

  it should "allow instantiation from abbreviations" in {
    Suit.fromAbbreviation('H') should equal (Suit.Heart)
    Suit.fromAbbreviation('h') should equal (Suit.Heart)
    Suit.fromAbbreviation('S') should equal (Suit.Spade)
    Suit.fromAbbreviation('D') should equal (Suit.Diamond)
    Suit.fromAbbreviation('C') should equal (Suit.Club)
    Suit.fromAbbreviation('J') should equal (Suit.Joker)
    an [IllegalArgumentException] should be thrownBy Suit.fromAbbreviation('X')
  }
}

