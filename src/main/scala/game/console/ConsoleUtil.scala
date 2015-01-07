package com.louis.fortypoints.game.console

import com.louis.fortypoints.game._
import com.louis.fortypoints.game.command._
import com.louis.fortypoints.card.Card

import scala.util.{Try, Success, Failure}

/**
 * Functions for the console version of the game.
 */
object ConsoleUtil {
  /**
   * Get the instructions displayed before user input on a given phase
   */

  private def mkLines(strings: String*): String = strings filter { !_.isEmpty } mkString "\n"

  def getPrePhaseDisplay(state: GameState): String = {
    val prettyHand: String = state.currentPlayer.hand map (_.toString) mkString ";"
    val currentHand = "Your current hand (Player %d): %s".format(state.currentTurn, prettyHand)

    // String for the winner, if applicable
    val winner = state.roundWinner match {
      case Some((p, play)) => "The winning hand is %s (Player %d)".format(play, p)
      case None => ""
    }

    state.phase match {
      case HouseSelection => "[UNIMPLEMENTED] Automatically selecting the house for now..." // TODO(louisli)
      case HandDrawing => "Drawing a card..."
      case HandSelectTrump => currentHand
      case HouseBottomFilter => 
        mkLines("Bottom cards: " + (state.deck.cards mkString " "), currentHand)
      case RoundFirstTurn | RoundOtherTurn => {
        mkLines(currentHand, "Board: " + (state.getPlays mkString " "), winner)
      }
      case RoundEnd => "Computing the winner..."
      case CountPoints => mkLines(winner, "[UNIMPLEMENTED] Count points")
      case GameEnd => {
        val (houseWon, oppPoints) = state.houseWon.get
        val (winStr, winPlayers) = if (houseWon) ("House", state.teamHouse) else ("Opposition", state.teamOpp)
        mkLines(
          "The game has ended. The opposition collected %d/%d points".format(oppPoints, state.pointThreshold), 
          "%s team won (Players %s)".format(winStr, winPlayers mkString "&"))
      }
    }
  }

  /**
   * Parse a semicolon separated list of strings into cards
   *
   * e.g. "Ah; Ax; Big Joker; Td"
   *
   * @return A list of cards, otherwise an exception result if one of the
   *         strings is an invalid card abbreviation
   */
  private def parseCards(line: String) : Try[List[Card]] = {
    Try(line.trim.split(";").toList map { _.trim } map { Card(_) })
  }

  /**
   * Infer the appropriate command from the command line
   * based on the current game phase
   */
  def parseCommand(raw: String, state: GameState): Either[CommandErrorStatus, Command] = {
    raw match {
      case "" => Right(BlankCommand)
      case "exit" => Right(ExitCommand)
      case _ => {
        // Assume the string is a list of cards
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
  }

  /**
   * Make a string red when printed to console
   */
  def red(s: String): String = {
    Console.RED + s + Console.RESET
  }

}
