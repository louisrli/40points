package com.louis.fortypoints.game.command

import org.scalatest._
import com.louis.fortypoints.UnitSpec
import com.louis.fortypoints.card._
import com.louis.fortypoints.game._
import com.louis.fortypoints.game.command._

class SetTrumpSpec extends UnitSpec with BeforeAndAfter {
  val state = GameTestUtil.blankState.copy(phase = HandSelectTrump)
  val testSuit = Suit.Heart
  val testTurn = 0

  /**
   * Checks if a state is reprompting for trump selection
   */
  private def checkReprompt(s: GameState) = {
    s.phase shouldBe HandSelectTrump
    s.currentTurn shouldBe testTurn;
  }

  before {
    state.phase shouldBe HandSelectTrump
  }

  "SetTrump command" should "set the trump if no trump suit has been set" in {
    val cmd = SetTrump(testTurn, List(Card(state.trumpRank, testSuit)))

    state.trumpSuit shouldBe None
    val newState = cmd.exec(state)
    newState.trumpSuit shouldBe Some(testSuit)
    newState.phase shouldBe HandDrawing
    newState.currentTurn shouldBe (testTurn + 1)
  }

  it should "fail and reprompt if the card is not of the trump rank" in {
    val cmd = SetTrump(testTurn, List(Card(Rank.Three, testSuit)))
    state.trumpSuit shouldBe None
    val newState = cmd.exec(state)

    // Should remain unchanged
    newState.trumpSuit shouldBe None
    newState.error shouldBe a [CommandTrumpWrongRank]
    checkReprompt(newState)
  }

  it should "fail and reprompt if a single trump has already been set" in {
    val cmd = SetTrump(testTurn, List(Card(state.trumpRank, Suit.Diamond)))

    val setState = state.copy(trumpSuit = Some(testSuit))
    setState.trumpSuit shouldBe (Some(testSuit))

    val newState = cmd.exec(setState)
    newState.trumpSuit shouldBe (Some(testSuit))
    newState.error shouldBe CommandTrumpAlreadySet
    checkReprompt(newState)
  }

  it should "fail and reprompt if multiple cards are selected (until multicard is implemented)" in {
    val cmd = SetTrump(testTurn, List(Card(state.trumpRank, testSuit), Card(state.trumpRank, testSuit)))

    state.trumpSuit shouldBe None

    val newState = cmd.exec(state)
    newState.trumpSuit shouldBe None
    newState.error shouldBe a [CommandTrumpWrongArity]
    checkReprompt(newState)
  }
}

