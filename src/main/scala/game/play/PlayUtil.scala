package com.louis.fortypoints.game.play

import com.louis.fortypoints.card._
import com.louis.fortypoints.game._

/**
 * Utility functions for plays [[com.louis.fortypoints.game.Play]]
 *
 * These functions determine things such as the type of hand (single, pair, etc.)
 * being played, winners of rounds, etc.
 */
case class PlayUtil(trumpRank: Rank.Value, trumpSuit: Suit.Value) {
  /**
   * Validate whether the first play of a round is legal
   *
   * @param play The current play
   * @return None if the play is legal,
   *         otherwise a [[com.louis.fortypoints.game.play.PlayValidationError]] 
   *         with a description of the error.
   */
  def validateFirstPlay(play: Play) : Option[PlayValidationError] = {
    // TODO: Do something with trump suit?
    None
  }

  /**
   * Validate whether a non-leading play (not the first play of the round) is legal
   * based on the input parameters.
   *
   * @param play The current play
   * @param hand The hand of the current player
   * @param firstPlay The first play of the round
   *
   * @return None if the play is legal, 
   *         otherwise a [[com.louis.fortypoints.game.play.PlayValidationError]] 
   *         with a description of the error.
   */
  def validateOtherPlay(
    play: Play, 
    hand: Hand,
    firstPlay: Play) : Option[PlayValidationError] = {
    if (play.cards.size != firstPlay.cards.size)
      Some(SizeError(firstPlay.cards.size, play.cards.size))
    else
      (firstPlay.rank, play.rank) match {
        case (Single(firstCard), Single(currentCard)) =>
          (isTrump(firstCard), isTrump(currentCard)) match {
            case (true, true) => None
            case (true, false) => // must be out of trumps
              if (!hasTrump(hand)) None else Some(HasTrumpsError())
            case (false, true) | (false, false) => // must be out of first suit
              val noFirstSuit = !hasNonTrump(hand, firstCard.suit)
              if (noFirstSuit)
                None 
              else if (firstCard.suit == currentCard.suit && !isTrump(currentCard))
                None
              else 
                Some(HasSuitError(firstCard.suit))
          }
      }
  }

  /**
   * Compare two plays and return the one with the higher rank
   *
   * If two plays are of equal rank, then default to returning p1. 
   * This corresponds with the assumption that p1 was played before p2.
   *
   * The two plays must have the same card size.
   * @param p1 First play to compare
   * @param p2 Second play to compare
   * @param first The first play of the round
   * @return Either p1 or p2, the winning play
   */
  def compareRank(p1: Play, p2: Play, first: Play) : Play = {
    (p1.rank, p2.rank, first.rank) match {
      case (Single(c1), Single(c2), Single(f)) =>
        // Return the play of higher rank (same suits)
        // p1 if equal (preserves order)
        val maxp = (r1: Rank.Value, r2: Rank.Value) => if (lt(r1, r2)) p2 else p1

        // Compare based on cases of trump and nontrump
        (isTrump(c1), isTrump(c2)) match {
          case (true, true) => 
            // Handle suit order of trump rank -- e.g., trump is 6c, 6c vs 6d
            if (c1.rank == trumpRank && c2.rank == trumpRank)
              if (c1.suit == trumpSuit || c2.suit != trumpSuit)
                p1  // Prioritize trump suit or whatever was played first
              else  // c2 is trump
                p2
            else
              maxp(c1.rank, c2.rank)
          case (true, false) => p1
          case (false, true) => p2
          case (false, false) => 
            // Two non-trumps depend on which one matches first suit
            (c1.suit == f.suit, c2.suit == f.suit) match {
              case (true, true) => maxp(c1.rank, c2.rank)
              case (false, false) => 
                if (c1.suit == c2.suit)
                  maxp(c1.rank, c2.rank)
                else
                  p1  // doesn't matter
              case (true, false) => p1
              case (false, true) => p2
            }
        }
    }
  }

  /**
   * List of the ranks for forty points in order, with the trump rank removed
   * and placed between Ace and LittleJoker
   */
  private val RankOrder: List[Rank.Value] = (List(
    Rank.Two, Rank.Three, Rank.Four, Rank.Five,
    Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine,
    Rank.Ten, Rank.Jack, Rank.Queen, 
    Rank.King, Rank.Ace) diff List(trumpRank)) ++
    List(trumpRank, Rank.LittleJoker, Rank.BigJoker)

  /**
   * Compare card ranks, specific for 40 points (ace is the highest)
   */
  private def lt(r1: Rank.Value, r2: Rank.Value) : Boolean = {
    // Search RankOrder for the first match. This linear in size of a constant list.
    // Could be slightly improved if we want.
    val (i, j) = (RankOrder.indexOf(r1), RankOrder.indexOf(r2))
    require (i != -1 && j != -1)  // shouldn't happen if RankOrder has all
    i < j
  }

  /**
   * Determine the winning play of a round
   *
   * @param plays A list of the plays in the order that they were played.
   *              The order is important, since the first play is used
   *              in determining the winning play.
   */
  def determineWinner(plays: List[Play]) : Play = {
    plays.foldLeft(plays.head)(compareRank(_, _, plays.head))
  }

  /**
   * Returns true if the hand has a nontrump card of the given suit.
   */
  def hasNonTrump(hand: Hand, suit: Suit.Value) : Boolean = {
    hand exists (c => !isTrump(c) && c.suit == suit)
  }

  /**
   * Returns true if the hand has any trump cards
   */
  def hasTrump(hand: Hand) : Boolean = {
    hand exists isTrump
  }

  /**
   * Returns true if the card is a trump card.
   */
  def isTrump(card: Card) : Boolean = {
    card match {
      case Card.LittleJoker | Card.BigJoker =>
        true
      case c if c.rank == trumpRank || c.suit == trumpSuit =>
        true
      case _ =>
        false
    }
  }

}

object PlayUtil {
  /**
   * Given a list of cards, determine the unique rank
   * (single, pair, etc.) of the cards
   *
   * @return An instance of [[com.louis.fortypoints.game.play.PlayRank]]
   */
  def determineRank(cards: List[Card]) : PlayRank = {
    cards.size match {
      case 1 => Single(cards.head)
      // TODO(louisli): Support other types of hands
      case _ => ???
    }
  }
}

