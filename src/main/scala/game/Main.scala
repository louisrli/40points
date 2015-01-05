package com.louis.fortypoints.game

import com.louis.fortypoints.card._

import scalaz.effect.IO


object Main {

  // TODO move these
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

  def putStrLnM(s: String): Game[Unit] = liftIO(IO.putStrLn(s))
  def readLnM: Game[String] = liftIO(IO.readLn)

  def mainLoop: Game[Unit] = for {
    // TODO(louisli) getCommandPrompt(gameState)
    _ <- liftIO(IO.putStrLn("Command based on game phase"))
    raw <- liftIO(IO.readLn)
    eitherCmd <- getCommandM(raw) 
    _ <- eitherCmd match {
      case Left(cmdError) => 
        liftIO(IO.putStr("There was an error: " + cmdError.toString))
      case Right(cmd) => for {
        // Process the command by updating state and printing out information to console
        _ <- liftIO(IO.putStr("exec cmd"))
        newState <- updateM(cmd)
        _ <- putStrLnM(newState.toString)
      } yield Unit
    }
    _ <- mainLoop
  } yield Unit

  def runGame[A](game: Game[A], state: GameState) = {
    game.eval(state).unsafePerformIO()
  }

  def game: Game[Unit] = mainLoop

  def main(args: Array[String]) {
    val initialState: GameState = GameState(
      players = Vector(new Player(), new Player(), new Player(), new Player()), 
      pointThreshold = 10,
      trumpSuit = None,
      trumpRank = Rank.Two,
      house = None,
      deck = Deck.getEmptyDeck,
      currentTurn = 0,
      phase = HouseSelection,
      pendingCalledCards = List(),
      teamHouse = List(),
      teamOpp = List(),
      houseWon = None)
    
    runGame(game, initialState)
  }
}
