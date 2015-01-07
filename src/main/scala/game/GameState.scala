package com.louis.fortypoints.game

import com.louis.fortypoints.card._
import com.louis.fortypoints.game.play._
import com.louis.fortypoints.game.command._

/**
 * Immutable class representing the current state of the game.
 * @param players 
 *  List of players
 * @param pointThreshold
 *  Number of points needed to win game
 * @param trumpSuit 
 *  Option of current trump suit. Will be None before it is determined or
 *  if game has no trump suit (latter unimplemented)
 * @param trumpRank
 *  Current trump rank.
 * @param house
 *  Option of index of house player. Will be None before it is determined.
 * @param deck
 *  The current deck of cards. After dealing, this will be the bottom cards.
 * @param currentTurn
 *  Index of player with current turn.
 * @param phase 
 *  [[com.louis.fortypoints.game.GamePhase]] representing the current
 *  stage of the game.
 *
 * @param pendingCalledCards
 *  List of cards called by the house
 * @param teamHouse
 *  List of players on the house team
 * @param teamOpp
 *  List of players on the opposing team. Should initially be all non-house members,
 *  changing as called cards are played.
 * @param houseWon
 *  An option representing whether the house team won. None if the game is not over yet.
 */
case class GameState(
  /* Properties determined at prologue */
  players: Vector[Player], 
  pointThreshold: Int,
  trumpSuit: Option[Suit.Value],
  trumpRank: Rank.Value,
  house: Option[Int],
  deck: Deck,

  /* Properties determined as game progresses */
  firstPlayer: Int,
  currentTurn: Int,
  phase: GamePhase,

  /* Properties related to teams */
  pendingCalledCards: List[Card],
  teamHouse: Team,
  teamOpp: Team,
  houseWon: Option[Boolean],

  error: CommandErrorStatus
) {
  /**
   * Returns a new game state with the current turn updated for
   * the next player, modulo the number of players
   */
  def nextTurn : GameState = {
    this.copy(currentTurn = (currentTurn + 1) % players.size)
  }

  /**
   * Returns the player with the current turn
   */
  def currentPlayer: Player = {
    this.players(this.currentTurn)
  }
  
  def updatePlayer(i: Int, p: Player): GameState = {
    this.copy(players = this.players.updated(i, p))
  }

  def setPlay(i: Int, p: Play): GameState = {
    this.updatePlayer(i, this.players(i).setPlay(p))
  }

  /**
   * Returns true if at least one play has been played so far this round.
   * Otherwise, returns false (no players have played yet).
   */
  def isRoundStarted: Boolean = {
    !(this.players flatMap { _.currentPlay } isEmpty)
  }

  /**
   * Returns the play (options) for the players
   */
  def getPlays: Vector[Option[Play]] = {
    this.players map { _.currentPlay }
  }

  /**
   * Returns the plays (with Nones removed) for the players
   */
  def getPlaysFlat: Vector[Play] = {
    this.players flatMap { _.currentPlay }
  }

  /**
   * Returns the plays in the order which they were played,
   * which could differ from the order of the players vector.
   */
  def getPlaysOrdered: Vector[Play] = {
    val (left, right) = this.players splitAt this.firstPlayer 
    Vector(right, left) flatMap {
      _.flatMap { _.currentPlay }
    }
  }

  /**
   * Clear the current plays of all the players, i.e. after the round has ended
   */
  def clearPlays: GameState = {
    this.copy(
      players = this.players map { _.copy(currentPlay = None) })
  }

  /**
   * Calculate the number of bottom cards for the game
   *
   * TODO(multideck): Needs to be revamped for multideck.
   */
  def numBottomCards: Int = {
    // It should be between 0.5 and 1.5 times the number of players
    val candidate = (54 % players.size)
    if (candidate > 0.5 * players.size) candidate else candidate + players.size
  }
}
