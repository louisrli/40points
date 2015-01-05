package com.louis.fortypoints

import scalaz.effect.IO
import scalaz.{StateT}

import com.louis.fortypoints.card.Card

package object game {
  // Typedefs for game logic
  type Team = List[Player]
  type Hand = List[Card]

  /**
   * Monadic type for the Game monad.
   * See MonadUtil for helper functions relating to lifting, etc.
   */
  type Game[A] = StateT[IO, GameState, A]
}
