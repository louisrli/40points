package com.louis.fortypoints.game.play

import org.scalatest._
import com.louis.fortypoints.UnitSpec
import com.louis.fortypoints.card._

class PlayUtilSpec extends UnitSpec with BeforeAndAfter {

  var playUtil: PlayUtil = _
  val trumpSuit : Suit.Value = Suit.Heart
  val trumpRank : Rank.Value = Rank.Two

  before {
    playUtil = new PlayUtil(trumpRank, trumpSuit)
  }

  "isTrump" should "return true for a card of trump suit" in {
    for (v <- Rank.getStandardRanks)
      playUtil.isTrump(Card(v, trumpSuit)) shouldBe true
  }

  it should "return true for a card of trump rank" in {
    for (v <- Suit.getStandardSuits)
      playUtil.isTrump(Card(trumpRank, v)) shouldBe true
  }

  it should "return false for non-trumps" in {
    playUtil.isTrump(Card("3d")) shouldBe false
    playUtil.isTrump(Card("Ac")) shouldBe false
    playUtil.isTrump(Card("Ts")) shouldBe false
  }

  it should "return true for jokers" in {
    playUtil.isTrump(Card.BigJoker) shouldBe true
    playUtil.isTrump(Card.LittleJoker) shouldBe true
  }

  "hasTrump" should "return true for a hand containing a trump card" in {
    playUtil.hasTrump(
      List(Card("3d"), Card("Ac"), Card("Ts"), Card("4h"))) shouldBe true
  }

  it should "return true for a hand containing multiple trump cards" in {
    playUtil.hasTrump(
      List(Card("3d"), Card("Little Joker"), Card("Ac"),
           Card("Ts"), Card("4h"))) shouldBe true
  }

  it should "return false for a hand containing no trump cards" in {
    playUtil.hasTrump(
      List(Card("3d"), Card("Ac"), Card("Ts"), Card("4c"))) shouldBe false
  }

  "hasNonTrump" should "return true for a hand containing a nontrump card" in {
    playUtil.hasNonTrump(
      List(Card("3h"), Card("4h"), Card("2d"), Card("4c")),
      Suit.Club) shouldBe true
  }

  it should "return true for a hand containing multiple nontrump cards" in {
    playUtil.hasNonTrump(
      List(Card("3h"), Card("4s"), Card("2d"), Card("4c")),
      Suit.Spade) shouldBe true
  }

  it should "return false for a hand containing no nontrump cards" in {
    playUtil.hasNonTrump(
      List(Card("3h"), Card("4h"), Card("2d"), Card("4h")),
      Suit.Diamond) shouldBe false
  }

  "Determining rank from a list of cards" should "identify singles" in {
    PlayUtil.determineRank(List(Card.BigJoker)) should be (Single(Card.BigJoker))
    PlayUtil.determineRank(List(Card("3h"))) should be (Single(Card("3h")))
  }

  // Convenience method for comparing three single cards
  private def compareSingle(p1s: String, p2s: String, fs: String) : Play = {
    val p1 = new Play(Card(p1s))
    val p2 = new Play(Card(p2s))
    val first = new Play(Card(fs))
    playUtil.compareRank(p1, p2, first)
  }

  private def testSingle(p1s: String, p2s: String, fs: String, expected: String) {
    compareSingle(p1s, p2s, fs) shouldBe (new Play(Card(expected)))
  }

  private def testSingleCommute(p1s: String, p2s: String, fs: String, expected: String) {
    testSingle(p1s, p2s, fs, expected)
    testSingle(p2s, p1s, fs, expected)
  }

  "Comparing the rank of two singles" should "compare two standard trumps" in {
    testSingleCommute("3h", "Ah", "3d", "Ah")
    testSingleCommute("7h", "6h", "3d", "7h")
    testSingleCommute("7h", "6h", "3h", "7h")
  }

  it should "compare a trump rank of nontrump suit with a nontrump rank of same suit" in {
    testSingleCommute("2d", "5d", "Ad", "2d")
    testSingleCommute("2d", "Td", "5h", "2d")
  }

  it should "compare a trump and nontrump with leading trump" in {
    testSingleCommute("7h", "4d", "3h", "7h")
    testSingleCommute("Little Joker", "4d", "3h", "Little Joker")
  }

  it should "compare a trump and nontrump with leading nontrump" in {
    testSingleCommute("Big Joker", "5s", "As", "Big Joker")
    testSingleCommute("7c", "6h", "Tc", "6h")
  }

  it should "compare two nontrumps with a leading nontrump, one of the same suit" in {
    testSingleCommute("9c", "8d", "5d", "8d")  // initiated with diamonds, clubs voided
    testSingleCommute("Ac", "As", "3s", "As")
    testSingleCommute("Ac", "Ks", "3c", "Ac")
  }

  it should "compare two nontrumps with a leading nontrump of a different suit" in {
    // We should expect it just to return the first one
    testSingle("Ac", "Kd", "5s", "Ac")
    testSingle("Kd", "Ac", "5s", "Kd")
  }

  it should "compare a joker and a trump" in {
    testSingleCommute("Big Joker", "2d", "5s", "Big Joker")
    testSingleCommute("Little Joker", "2s", "5s", "Little Joker")
    testSingleCommute("Little Joker", "Th", "5s", "Little Joker")
  }

  it should "compare a joker and a nontrump" in {
    testSingleCommute("Big Joker", "Ts", "5s", "Big Joker")
    testSingleCommute("Little Joker", "2c", "5s", "Little Joker")
  }

  private def mkPlay(strings: String*) : List[Play] = {
    strings.toList map { s => (new Play(Card(s))) }
  }

  private def testSingleWinner(expected: String, strings: String*) {
    playUtil.determineWinner(mkPlay(strings:_*)) should be (
      new Play(Card(expected)))
  }

  "determineWinner" should "identify the winner for a singles round" in {
    // trump first, all trumps
    testSingleWinner(expected = "Ah", "3h", "5h", "Ah", "Kh")
    testSingleWinner(expected = "Big Joker", "3h", "Big Joker", "Ah", "Kh")
    testSingleWinner(expected = "2d", "5h", "Th", "2d", "Kh")

    // trump first, nontrump
    testSingleWinner(expected = "2d", "Th", "5d", "2d", "8c")

    // non-trump first, non-trump only
    testSingleWinner(expected = "Ad", "Kd", "Ad", "As", "Ac")
    testSingleWinner(expected = "Kd", "Kd", "Jd", "Td", "3d")

    // non-trump first, one trump played
    testSingleWinner(expected = "2s", "Kd", "Jd", "2s", "3d")

    // non-trump first, multiple trump played
    testSingleWinner(expected = "Big Joker", "Kd", "Big Joker", "2s", "Little Joker")
  }

  "validateOtherPlay" should "validate other rounds with correctly (leading nontrump single)" in {
    val firstPlay = Play.fromAbbrevs("5d")

    val hasDiamondHand = Card.toHand("8s", "7d", "4d", "Little Joker")
    val hasNoDiamondHand = Card.toHand("2s", "3c", "4s", "5h")

    // nontrump, same suit (diamond)
    val p0 = Play.fromAbbrevs("7d")
    playUtil.validateOtherPlay(p0, hasDiamondHand, firstPlay) shouldBe None

    val p1 = Play.fromAbbrevs("5s")
    // nontrump, diff suit, but has diamond
    playUtil.validateOtherPlay(p1, hasDiamondHand, firstPlay) shouldBe (Some(HasSuitError(Suit.Diamond)))

    // nontrump, diff suit, but has no diamond
    playUtil.validateOtherPlay(p1, hasNoDiamondHand, firstPlay) shouldBe (None)

    val p2 = Play.fromAbbrevs("Big Joker")
    // trump, but has diamond
    playUtil.validateOtherPlay(p2, hasDiamondHand, firstPlay) shouldBe (Some(HasSuitError(Suit.Diamond)))

    // trump, but has no diamond
    playUtil.validateOtherPlay(p2, hasNoDiamondHand, firstPlay) shouldBe None
  }

  it should "validate other rounds correctly (leading trump single)" in {
    val firstPlay = new Play(Card("Big Joker"))

    val hasTrumpHand = Card.toHand("8s", "7h", "4d", "5s")
    val hasNoTrumpHand = Card.toHand("3c", "4s")

    val p1 = Play.fromAbbrevs("5s")
    // nontrump but has trump: error, need to play trump
    playUtil.validateOtherPlay(p1, hasTrumpHand, firstPlay) shouldBe (Some(HasTrumpsError()))

    // nontrump but has no trump: ok
    playUtil.validateOtherPlay(p1, hasNoTrumpHand, firstPlay) shouldBe None

    val p2 = Play.fromAbbrevs("Big Joker")
    // trump and has trump
    playUtil.validateOtherPlay(p2, hasTrumpHand, firstPlay) shouldBe None
  }

  "Default sort" should "sort hands correctly" in {
    // Assuming trump is 2h
    val h1 = Card.toHand("As", "6s", "2d", "Big Joker", "4d", "5c", "6c", "Ah")
    val e1 = Card.toHand("Ah", "2d", "Big Joker", "6s", "As", "4d", "5c", "6c")
    playUtil.sort(h1) shouldBe e1

    // no trumps
    val h2 = Card.toHand("As", "5c", "4c", "3d", "8c", "Ad", "3s")
    val e2 = Card.toHand("3s", "As", "3d", "Ad", "4c", "5c", "8c")
    playUtil.sort(h2) shouldBe e2

    val h3 = Card.toHand("As", "2s", "3d", "4d", "Kd")
    val e3 = Card.toHand("2s", "As", "3d", "4d", "Kd")
    playUtil.sort(h3) shouldBe e3

    playUtil.sort(Nil) shouldBe Nil

    val h5 = Card.toHand("2h")
    playUtil.sort(h5) shouldBe h5
  }

}

