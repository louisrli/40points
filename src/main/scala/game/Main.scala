package com.louis.fortypoints.game

import com.louis.fortypoints.card._

import scalaz.effect.IO
import com.louis.fortypoints.game.MonadUtil._

/**
 * Entry point for the console version of the game.
 */
object Main {
  /**
   * Recursively defined main loop for the Game monad
   */
  private def mainLoop: Game[Unit] = for {
    gameMode <- getInputModeM
    prePhaseDisplay <- getPrePhaseDisplayM
    _ <- putStrLnM(prePhaseDisplay)
    // Only request user input if this game mode needs user input
    _ <- gameMode match {
      case RequestInput(prompt) => {
        for {
          // Request and validate user input
          _ <- putStrM(prompt + ": ")
          raw <- readLnM
          _ <- putStrLnM(raw)
          eitherCmd <- getCommandM(raw) 
          _ <- eitherCmd match {
            case Left(cmdError) => 
              putStrLnM("There was an error: " + cmdError.toString)
            // Process user input, updating the state
            case Right(cmd) => for {
              newState <- updateM(cmd)
            } yield Unit
          }
        } yield Unit
      } 
      case NoInput => for {
        newState <- updateM(BlankCommand)
      } yield Unit
    }
    _ <- mainLoop
  } yield Unit

  /**
   * The game monad
   */
  def game: Game[Unit] = mainLoop

  /**
   * Run the game monad from the given state
   * @param game Game monad
   * @param state Initial state
   */
  def runGame[A](game: Game[A], state: GameState) = {
    game.eval(state).unsafePerformIO()
  }

  def main(args: Array[String]) {
    // TODO(louisli) revamp based on config possibly
    val initialState: GameState = GameState(
      players = Vector(new Player(), new Player(), new Player(), new Player()), 
      pointThreshold = 10,
      trumpSuit = None,
      trumpRank = Rank.Two,
      house = None,
      deck = Deck.getStandardDeckJoker,
      currentTurn = 0,
      phase = HouseSelection,
      pendingCalledCards = List(),
      teamHouse = List(),
      teamOpp = List(),
      houseWon = None)
    
    runGame(game, initialState)
  }
}
