package com.louis.fortypoints.game

import com.louis.fortypoints.card._

import scalaz.effect.IO
import com.louis.fortypoints.game.MonadUtil._
import com.louis.fortypoints.game.command._
import com.louis.fortypoints.game.console.ConsoleUtil._

/**
 * Entry point for the console version of the game.
 */
object Main {

  /**
   * Recursively defined main loop for the Game monad
   */
  private def mainLoop: Game[Unit] = for {
    // Check if the game has ended
    ended <- gameOverM
    _ <- if (ended) sys.exit(0) else unitM
    gameMode <- getInputModeM
    // Print headers at the beginning of the game phase
    prePhaseDisplay <- getPrePhaseDisplayM
    _ <- putStrLnM(prePhaseDisplay)

    // Request user input if needed
    _ <- gameMode match {
      case RequestInput(prompt) => {
        for {
          // Print out an error if exists
          error <- getErrorM
          _ <- if (error == CommandNoError) 
                unitM 
              else 
                putStrLnM(red("[ERROR] ") + CommandErrorStatus.getMessage(error))
          _ <- putStrM(prompt + ": ")

          // Request and validate user input
          raw <- readLnM
          eitherCmd <- getCommandM(raw) 
          _ <- eitherCmd match {
            case Left(cmdError) => 
              putStrLnM(red("[ERROR] ") + cmdError.toString)
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
    _ <- putStrLnM("")
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
      pointThreshold = 40,
      trumpSuit = Some(Suit.Spade),
      trumpRank = Rank.Two,
      house = 0,
      deck = Deck.getStandardDeckJoker,
      firstPlayer = 0,
      currentTurn = 0,
      roundWinner = None,
      phase = HouseSelection,
      teamHouse = List(),
      houseWon = None,
      error = CommandNoError)
    
    runGame(game, initialState)
  }
}
