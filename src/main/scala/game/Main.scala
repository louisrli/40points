package com.louis.fortypoints.game

import com.louis.fortypoints.card._

import scalaz.effect.IO


object Main {

  // TODO move these into some GameUtil
  def getCommandM(raw: String): Game[Either[CommandError, Command]] = {
    liftState[Either[CommandError, Command]] { (s: GameState) =>
      (s, Command.parseCommand(raw, s))
    }
  }

  def updateM(cmd: Command): Game[GameState] = {
    liftState[GameState] { (s: GameState) =>
      val newState = GameLoop.update(s, cmd)
      (newState, newState)
    }
  }

  def getGameModeM : Game[GameLoop.GameMode] = {
    liftState[GameLoop.GameMode] { (s: GameState) => (s, GameLoop.getGameMode(s.phase)) }
  }

  def putStrM(s: String): Game[Unit] = liftIO(IO.putStr(s))
  def putStrLnM(s: String): Game[Unit] = liftIO(IO.putStrLn(s))
  def readLnM: Game[String] = liftIO(IO.readLn)

  /**
   * Get the instructions displayed before user input on a given phase
   */
  def getPrePhaseDisplay(state: GameState): String = {
    val prettyHand: String = state.currentPlayer.hand map (_.toString) mkString ";"
    val currentHand = "Your current hand (%d): %s".format(state.currentTurn, prettyHand)
    state.phase match {
      case HouseSelection => "[UNIMPLEMENTED] Automatically selecting the house for now..." // TODO(louisli)
      case HandDrawing => "Drawing a card..."
      case HandSelectTrump => currentHand
      case HouseBottomFilter => "Bottom cards: " + state.deck.cards + "\n" + currentHand
      case HouseCallCards => "[UNIMPLEMENTED] Skipping house call cards for now..." // TODO(louisli)
      case RoundFirstTurn | RoundOtherTurn => currentHand
      case RoundEnd => "Ending the round" // TODO(louisli) to print the winner, this should really be after
      case CountPoints => "[UNIMPLEMENTED] Count points"
      case GameEnd => "Game end"
    }
  }

  def getPrePhaseDisplayM: Game[String] = {
    liftState[String] { (s: GameState) => (s, getPrePhaseDisplay(s)) }
  }

  def mainLoop: Game[Unit] = for {
    gameMode <- getGameModeM
    prePhaseDisplay <- getPrePhaseDisplayM
    _ <- putStrLnM(prePhaseDisplay)
    // Only request user input if this game mode needs user input
    _ <- gameMode match {
      case GameLoop.RequestInput(prompt) => {
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
      case GameLoop.ContinueGame => for {
        newState <- updateM(BlankCommand)
      } yield Unit
    }
    _ <- mainLoop
  } yield Unit

  def runGame[A](game: Game[A], state: GameState) = {
    game.eval(state).unsafePerformIO()
  }

  def game: Game[Unit] = mainLoop

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
