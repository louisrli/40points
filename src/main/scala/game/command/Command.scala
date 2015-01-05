package com.louis.fortypoints.game

import com.louis.fortypoints.card._

/**
 * Command abstracts a user input (it could come from the console, the web, etc.)
 *
 * Only certain game phases require commands to continue.
 */
sealed trait Command
case class SetTrump(player: Int, cards: List[Card]) extends Command
case class HouseFilterBottomCards(player: Int, cards: List[Card]) extends Command
case class MakePlay(player: Int, cards: List[Card]) extends Command
case object ExitCommand extends Command
case object BlankCommand extends Command
