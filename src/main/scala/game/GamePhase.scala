package com.louis.fortypoints.game

/**
 * An enumeration of the possible phases of the game.
 */
sealed trait GamePhase

/* Prologue */
case object HouseSelection extends GamePhase
case object HandDrawing extends GamePhase
case object HouseBottomFilter extends GamePhase
case object HouseCallCards extends GamePhase

/* Rounds -- (turn-based playing) */
case object RoundFirstTurn extends GamePhase
case object RoundOtherTurn extends GamePhase
case object RoundEnd extends GamePhase

/* Epilogue */
case object CountPoints extends GamePhase
case object GameEnd extends GamePhase

