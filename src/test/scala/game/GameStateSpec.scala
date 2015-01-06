package com.louis.fortypoints.game

import org.scalatest._
import com.louis.fortypoints.UnitSpec
import com.louis.fortypoints.card._
import com.louis.fortypoints.game.play._

class GameStateSpec extends UnitSpec with BeforeAndAfter {
  "A Game State" should "clear plays" in {
    var state = GameTestUtil.blankState
    for (i <- 0 to 3) {
      state = state.setPlay(i, GameTestUtil.getPlay)
    }

    state = state.clearPlays

    for (i <- 0 to 3) {
      state.players(i).currentPlay shouldBe None
    }
  }

  it should "be able to tell if the round has started" in {
    val state = GameTestUtil.blankState
    state.isRoundStarted shouldBe false

    val startedState = state.setPlay(0, GameTestUtil.getPlay)
    startedState.isRoundStarted shouldBe true
  }

  it should "get the plays in order based on the first player" in {
    val cards = List("2s", "3s", "4s", "5s")
    val plays = cards map { (s: String) => new Play(Card(s)) }

    for (split <- 0 to 3) {
      var state = GameTestUtil.blankState.copy(firstPlayer = split)

      for (i <- 0 to 3) {
        state = state.setPlay(i, plays(i))
      }

      for (i <- 0 to 3) {
        state.getPlaysOrdered(i) shouldBe (plays((i + split) % 4))
      }
    }
  }

  it should "increment the turn in a cyclic manner" in {
    val state = GameTestUtil.blankState
    state.currentTurn shouldBe 0

    val s1 = state.nextTurn
    s1.currentTurn shouldBe 1

    val s2 = s1.nextTurn
    s2.currentTurn shouldBe 2

    val s3 = s2.nextTurn
    s3.currentTurn shouldBe 3

    s3.nextTurn.currentTurn shouldBe 0
  }
}

