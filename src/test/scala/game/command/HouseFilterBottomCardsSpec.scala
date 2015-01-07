package com.louis.fortypoints.game.command

import org.scalatest._
import com.louis.fortypoints.UnitSpec
import com.louis.fortypoints.card._
import com.louis.fortypoints.game._
import com.louis.fortypoints.game.command._

class HouseFilterBottomCardsSpec extends UnitSpec with BeforeAndAfter {
  val state = GameTestUtil.dealtState.copy(phase = HouseBottomFilter)

  /**
   * Checks if a state is reprompting for trump selection
   */
  private def checkReprompt(s: GameState) = {
    s.deck.cards shouldBe state.deck.cards  // deck should be unchanged
    s.phase shouldBe HouseBottomFilter
    s.currentTurn shouldBe state.house;
    s.currentPlayer.hand should not be 'empty
  }

  before {
    checkReprompt(state)
  }

  "HouseFilterBottomCards command" should "swap bottom cards when arity and membership are satisfied" in {
    // Trivial case: input cards are just the bottom
    val trivialCmd = new HouseFilterBottomCards(state.house, state.deck.cards)

    val trivialState = trivialCmd.exec(state)

    // Bottom and hand should be unchanged
    trivialState.deck.cards shouldBe state.deck.cards
    trivialState.currentPlayer.hand shouldBe state.currentPlayer.hand
    trivialState.error shouldBe CommandNoError
    trivialState.phase shouldBe RoundFirstTurn

    // Split case: take some from bottom, some from hand
    val fromBottom = state.deck.cards.take(2)
    val fromHand = state.currentPlayer.hand.take(state.numBottomCards - 2)
    val splitCmd = new HouseFilterBottomCards(state.house, fromBottom ++ fromHand)

    val splitState = splitCmd.exec(state)
    // Check that the bottom is correct
    splitState.deck.cards should contain theSameElementsAs (fromBottom ++ fromHand)
    // TODO(multideck): this test could break
    (splitState.currentPlayer.hand diff (fromBottom ++ fromHand)) shouldBe splitState.currentPlayer.hand
    splitState.error shouldBe CommandNoError
    splitState.phase shouldBe RoundFirstTurn

    // Hand case: take all from hand
    val inputHand = state.currentPlayer.hand.take(state.numBottomCards)
    val handCmd = new HouseFilterBottomCards(state.house, inputHand)

    val handState = handCmd.exec(state)
    handState.deck.cards should contain theSameElementsAs inputHand
    (handState.currentPlayer.hand diff inputHand) shouldBe handState.currentPlayer.hand
    splitState.error shouldBe CommandNoError
    splitState.phase shouldBe RoundFirstTurn
  }

  it should "fail and reprompt when arity is incorrect" in {
    val inputHand = state.currentPlayer.hand.take(state.numBottomCards + 1)
    val cmd = new HouseFilterBottomCards(state.house, inputHand)

    val newState = cmd.exec(state)
    newState.error shouldBe a [CommandBottomWrongArity]
    checkReprompt(newState)
  }

  it should "fail and reprompt when an input card is not in the bottom or in the hand" in {
    // To get a card not in the house's hand, let's take a card from another player's hand
    val otherCard = state.nextTurn.currentPlayer.hand.head
    val inputHand = otherCard :: state.currentPlayer.hand.take(state.numBottomCards).tail
    val cmd = new HouseFilterBottomCards(state.house, inputHand)

    val newState = cmd.exec(state)

    newState.error shouldBe a [CommandBottomInvalidCards]
    checkReprompt(newState)
  }

  it should "fail if the command comes from a non-house player" in {
    val cmd = new HouseFilterBottomCards(
      state.nextTurn.currentTurn, 
      state.currentPlayer.hand.take(state.numBottomCards))

    val newState = cmd.exec(state)
    newState.error shouldBe CommandInvalidPlayer
    checkReprompt(newState)
  }
}

