package com.louis.fortypoints.game.command

import org.scalatest._
import com.louis.fortypoints.UnitSpec
import com.louis.fortypoints.card._
import com.louis.fortypoints.game._
import com.louis.fortypoints.game.play._
import com.louis.fortypoints.game.command._

class MakePlaySpec extends UnitSpec with BeforeAndAfter {
  val turn = 1
  val hand0 = Card.toHand("Ah", "2h", "3h", "4h", "5h", "Big Joker")
  val hand1 = Card.toHand("Ad", "2d", "7h", "8h", "9h", "Little Joker")

  val state = GameTestUtil.dealtTrumpState
  val stateUnplayed = state
    .copy(phase = RoundFirstTurn, currentTurn = 0)
    .updatePlayer(0, state.players(0).copy(hand = hand0))
    .updatePlayer(1, state.players(1).copy(hand = hand1))

  val stateFirstPlay = MakePlay(0, Card.toHand("Ah"))
    .exec(stateUnplayed)
    .copy(phase = RoundOtherTurn)

  /**
   * Checks if a state is reprompting for making a play for the other play case only
   */
  private def checkReprompt(s: GameState) = {
    s.phase shouldBe RoundOtherTurn
    s.currentTurn shouldBe stateFirstPlay.currentTurn;
    s.currentPlayer.hand shouldBe stateFirstPlay.currentPlayer.hand
    s.currentPlayer.currentPlay shouldBe None
  }

  before {
    checkReprompt(stateFirstPlay)
  }

  "A MakePlay command" should "update the player's current play if the play is valid" in {
    stateFirstPlay.currentTurn shouldBe turn
    stateFirstPlay.players(turn).currentPlay shouldBe None

    val cmd = MakePlay(turn, Card.toHand("7h"))
    val updated = cmd.exec(stateFirstPlay)
    updated.currentTurn shouldBe (turn + 1)
    updated.players(turn).currentPlay shouldBe Some(new Play(Card.toHand("7h")))
    updated.error shouldBe CommandNoError
  }

  it should "allow any single to be played as the first play" in {
    stateUnplayed.players(0).currentPlay shouldBe None
    for (card <- hand0) {
      val cmd = MakePlay(0, List(card))
      val updated = cmd.exec(stateUnplayed)
      updated.players(0).currentPlay shouldBe Some(new Play(List(card)))
      updated.error shouldBe CommandNoError
    }
  }

  it should "fail and reprompt if the player does not have the cards" in {
    // Let player 1 be the current player, and try player 0's hand cards
    for (card <- hand0) {
      val cmd = MakePlay(turn, List(card))
      val updated = cmd.exec(stateFirstPlay)
      checkReprompt(updated)
      updated.error shouldBe a [CommandPlayInvalidCards]
    }
  }

  it should "fail and reprompt if the play is invalid" in {
    val cmd = MakePlay(turn, hand1.take(3))
    val updated = cmd.exec(stateFirstPlay)
    checkReprompt(updated)
    updated.error shouldBe a [CommandPlayError]
  }

  it should "fail and reprompt if the command comes from the wrong player" in {
    val cmd = MakePlay(turn + 1, List(state.players(turn + 1).hand.head))
    val updated = cmd.exec(stateFirstPlay)
    updated.error shouldBe CommandInvalidPlayer
  }

}

