package com.louis.fortypoints.game.command

import com.louis.fortypoints.card._
import com.louis.fortypoints.game._
import com.louis.fortypoints.game.play._

sealed trait CommandErrorStatus
case class CommandInvalidPhase(phase: GamePhase) extends CommandErrorStatus
case class CommandException(e: Throwable) extends CommandErrorStatus
case class CommandTrumpWrongArity(actual: Int, expected: Int) extends CommandErrorStatus
case class CommandTrumpWrongRank(actual: Rank.Value, expected: Rank.Value) extends CommandErrorStatus
case object CommandTrumpAlreadySet extends CommandErrorStatus
case class CommandPlayError(pve: PlayValidationError) extends CommandErrorStatus
case class CommandPlayInvalidCards(invalid: List[Card]) extends CommandErrorStatus
case class CommandBottomWrongArity(actual: Int, expected: Int) extends CommandErrorStatus
case class CommandBottomInvalidCards(invalid: List[Card]) extends CommandErrorStatus

/**
 * Generic error for a command received out of turn
 */
case object CommandInvalidPlayer extends CommandErrorStatus

/**
 * Generic error for a command received in the wrong phase
 */
case object CommandInvalidConfig extends CommandErrorStatus

case object CommandNoError extends CommandErrorStatus

object CommandErrorStatus {
  def getMessage: CommandErrorStatus => String = {
    case CommandInvalidPhase(phase) => {
      "A command was sent during the wrong phase of the game: %s (this shouldn't happen)."
        .format(phase)
    }
    case CommandException(e) => "There was an exception: " + e.toString
    case CommandTrumpWrongArity(actual, expected) => 
      "Please select %d trump cards (you selected %d)".format(actual, expected)
    case CommandTrumpWrongRank(actual, expected) => {
      "The trump rank is %s, but you selected a card of rank %s"
        .format(actual, expected)
    }
    case CommandTrumpAlreadySet =>
      "The trump card has already been set"
    case CommandPlayError(pve) => PlayValidationError.getMessage(pve)
    case CommandPlayInvalidCards(invalid) => 
      "Your hand does not contain the cards: %s".format(invalid mkString ";")
    case CommandBottomWrongArity(actual, expected) =>
      "Please select %d bottom cards (you selected %d)".format(actual, expected)
    case CommandBottomInvalidCards(invalid) =>
      "Your hand does not contain the cards: %s".format(invalid mkString ";")
    case CommandInvalidPlayer =>
      "It is not your turn right now (this shouldn't happen)."
    case CommandInvalidConfig =>
      "Found an invalid game configuration (this shouldn't happen)."
    case CommandNoError => ""
  }
}
