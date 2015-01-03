package com.louis.fortypoints.game

/**
 * An enumeration of the possible phases of the game.
 */
sealed trait GamePhase

/* Prologue */
case class HouseSelection() extends GamePhase
case class HandDrawing() extends GamePhase
case class HouseBottomFilter() extends GamePhase
case class HouseCallCards() extends GamePhase

/* Rounds -- (turn-based playing) */
case class RoundFirstTurn() extends GamePhase
case class RoundOtherTurn() extends GamePhase
case class RoundEnd() extends GamePhase

/* Epilogue */
case class CountPoints() extends GamePhase
case class GameEnd() extends GamePhase

