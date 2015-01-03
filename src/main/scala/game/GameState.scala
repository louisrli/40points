package com.louis.fortypoints.game

import com.louis.fortypoints.card._

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
  currentTurn: Int,
  phase: GamePhase,

  /* Properties related to teams */
  pendingCalledCards: List[Card],
  teamHouse: Team,
  teamOpp: Team,
  houseWon: Option[Boolean]
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
  def currentPlayer = {
    this.players(this.currentTurn)
  }

  def updatePlayer(i: Int, p: Player) = {
    this.copy(players = this.players.updated(i, p))
  }

  /**
   * Clear the current plays of all the players, i.e. after the round has ended
   */
  def clearPlays = {
    this.copy(
      players = this.players map { _.copy(currentPlay = None) })
  }
}
