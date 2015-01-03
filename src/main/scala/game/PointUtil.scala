package com.louis.fortypoints.game

import com.louis.fortypoints.card._

/**
 * Utility functions for computations on cards: point calculations
 */
object PointUtil {
  private val TenPointRanks = Set(Rank.Ten, Rank.King)
  private val FivePointRanks = Set(Rank.Five)
  private val PointRanks = TenPointRanks union FivePointRanks

  /**
   * Returns true if a  [[com.louis.fortypoints.card.Card]] is a point card
   */
  def isPoint(card: Card) : Boolean = {
    PointRanks contains card.rank
  }

  /**
   * Returns the point value for a [[com.louis.fortypoints.card.Card]]
   */
  def pointValue(card: Card) : Int = {
    card.rank match {
      case rank if TenPointRanks contains rank => 10
      case rank if FivePointRanks contains rank => 5
      case _ => 0
    }
  }

  /**
   * Sums the total points for a team
   */
  def tallyTeamPoints(team: Team) : Int = {
    team.foldLeft(0)(_ + _.tallyPoints)
  }
}

