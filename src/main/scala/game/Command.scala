package com.louis.fortypoints.game

import com.louis.fortypoints.card._
import scala.util.{Try, Success, Failure}

sealed trait Command
case class SetTrump(player: Int, cards: List[Card]) extends Command
case class HouseFilterBottomCards(player: Int, cards: List[Card]) extends Command
case class MakePlay(player: Int, cards: List[Card]) extends Command
case object BlankCommand extends Command

sealed trait CommandError
case class CommandInvalidPhase(phase: GamePhase) extends CommandError
case class CommandException(e: Throwable) extends CommandError

object Command {
  /**
   * Parse a semicolon separated list of strings into cards
   *
   * e.g. "Ah; Ax; Big Joker; Td"
   *
   * @return A list of cards, otherwise an exception result if one of the
   *         strings is an invalid card abbreviation
   */
  def parseCards(line: String) : Try[List[Card]] = {
    Try(
      line.trim.split(";").toList map { _.trim } map { Card(_) })
  }

  /**
   * Infer the appropriate command based on the current game phase
   */
  def parseCommand(raw: String, state: GameState): Either[CommandError, Command] = {
    parseCards(raw) match {
      case Success(cards) => {
        state.phase match {
          case HandDrawing => Right(SetTrump(state.currentTurn, cards))
          case HouseBottomFilter => Right(HouseFilterBottomCards(state.currentTurn, cards))
          case RoundFirstTurn | RoundOtherTurn => Right(MakePlay(state.currentTurn, cards))
          case _ => Left(CommandInvalidPhase(state.phase))
        }
      }
      case Failure(e) => Left(CommandException(e))
    }
  }
}
