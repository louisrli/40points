package com.louis.fortypoints

import scalaz.effect.IO
import scalaz.{Free,StateT}
import scalaz.concurrent.Task


import com.louis.fortypoints.card.Card

package object game {
  // Typedefs for game logic
  type Team = List[Player]
  type Hand = List[Card]

  // Typedefs for functional game loop
  type Game[A] = StateT[IO, GameState, A]

  // TODO move
  def liftIO[A](io: IO[A]): Game[A] = {
    StateT[IO, GameState, A](s => io.map(a => (s, a)))
  }

  def liftState[A](f: GameState => (GameState, A)): Game[A] = {
    StateT[IO, GameState, A] { (s) => IO(f(s)) }
  }
}
