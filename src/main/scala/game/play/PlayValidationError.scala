package com.louis.fortypoints.game.play

import com.louis.fortypoints.card.Suit

sealed trait PlayValidationError
case class SizeError(expected: Int, actual: Int) extends PlayValidationError
case class HasTrumpsError() extends PlayValidationError
case class HasSuitError(suit: Suit.Value) extends PlayValidationError
