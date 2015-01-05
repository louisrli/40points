package com.louis.fortypoints.game

import com.louis.fortypoints.game.console.ConsoleUtil
import com.louis.fortypoints.game.command._

import scalaz.effect.IO
import scalaz.{StateT}

/**
 * Utility functions for working with the Game monad.
 *
 * Primarily lifted versions of functions elsewhere in the program. If your
 * goal is to understand how the monadic loop works, then look here. If you
 * want to look at detailed implementations of the game, then look inside of
 * the functions called by the lifted versions.
 */
object MonadUtil {
  /**
   * Lift an IO monad to a Game monad.
   */
  def liftIO[A](io: IO[A]): Game[A] = {
    StateT[IO, GameState, A](s => io.map(a => (s, a)))
  }

  /**
   * Lift a function (representing a function used in a State monad)
   * to a Game monad.
   */
  def liftState[A](f: GameState => (GameState, A)): Game[A] = {
    StateT[IO, GameState, A] { (s) => IO(f(s)) }
  }

  /**
   * Lifted function for parsing commands in Game monad
   */
  def getCommandM(raw: String): Game[Either[CommandError, Command]] = {
    liftState[Either[CommandError, Command]] { (s: GameState) =>
      (s, ConsoleUtil.parseCommand(raw, s))
    }
  }

  /**
   * Lifted function for updating the state with a command in Game monad
   */
  def updateM(cmd: Command): Game[GameState] = {
    liftState[GameState] { (s: GameState) =>
      val newState = FortyPointsGame.update(s, cmd)
      (newState, newState)
    }
  }

  /**
   * Lifted function for retrieving the state in Game monad
   */
  def getInputModeM : Game[InputMode] = {
    liftState[InputMode] { (s: GameState) => (s, InputMode.getInputMode(s.phase)) }
  }

  //
  // Below this line
  // CONSOLE VERSION ONLY
  //

  /**
   * Lifted version of IO.putStr to Game monad
   * Print a string
   */
  def putStrM(s: String): Game[Unit] = liftIO(IO.putStr(s))

  /**
   * Lifted version of IO.putStrLn to Game monad
   * Print a string with a newline
   */
  def putStrLnM(s: String): Game[Unit] = liftIO(IO.putStrLn(s))

  /**
   * Lifted version of IO.readLn to Game monad
   * Read a string from input
   */
  def readLnM: Game[String] = liftIO(IO.readLn)

  /**
   * Lifted version of the text displayed on console before a user provides
   * input, displaying information such as their current hand.
   */
  def getPrePhaseDisplayM: Game[String] = {
    liftState[String] { (s: GameState) => (s, ConsoleUtil.getPrePhaseDisplay(s)) }
  }

}

