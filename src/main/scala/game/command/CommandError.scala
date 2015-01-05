package com.louis.fortypoints.game.command

import com.louis.fortypoints.game._

sealed trait CommandError
case class CommandInvalidPhase(phase: GamePhase) extends CommandError
case class CommandException(e: Throwable) extends CommandError
