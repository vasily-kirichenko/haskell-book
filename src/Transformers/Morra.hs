module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           System.Random

data Scores =
  Scores {
    player   :: Int
  , computer :: Int
  } deriving Show

data GameState =
  GameState {
    scores           :: Scores
  , allPlayerGuesses :: [Int]
} deriving Show

getComputerGuess :: Int -> StateT GameState IO Int
getComputerGuess maxNumber = do
  liftIO $ randomRIO (0, maxNumber)

getComputerFingers :: IO Int
getComputerFingers = randomRIO (0, 5)

askPlayerForNumber :: String -> IO Int
askPlayerForNumber caption =
  putStr (caption ++ ": ") >> readLn :: IO Int

game :: StateT GameState IO ()
game = do
  playerGuess <- liftIO $ askPlayerForNumber "\nYour guess"
  computerGuess <- getComputerGuess 10
  liftIO . putStrLn $ "Computer guess: " ++ show computerGuess
  playerFingers <- liftIO $ askPlayerForNumber "How many fingers? "
  computerFingers <- getComputerGuess 5
  liftIO . putStrLn $ "Computer showed " ++ show computerFingers ++ " fingers."
  let totalFingers = playerFingers + computerFingers
  liftIO . putStrLn $ "Total fingers = " ++ show totalFingers
  state <- get
  let oldScores = scores state

  newScores <- liftIO $
    if totalFingers == playerFingers
       && totalFingers == computerFingers then do
      putStrLn "Both guess right!"
      return $ Scores (player oldScores + 1) (computer oldScores + 1)
    else if totalFingers == playerGuess then do
      putStrLn "Your guess was right!"
      return $ oldScores { player = player oldScores + 1 }
    else if totalFingers == computerGuess then do
      putStrLn "Computer guess was right, sorry."
      return $ oldScores { computer = computer oldScores + 1 }
    else do
      putStrLn "Nobody guessed."
      return oldScores

  let newState = GameState newScores (playerGuess : allPlayerGuesses state)
  liftIO . putStrLn $ "Current state: " ++ show newState
  put newState

  if player newScores == 3 then
    liftIO $ putStrLn "You win!"
  else if computer newScores == 3 then
    liftIO $ putStrLn "Computer wins, sorry."
  else game

main :: IO ()
main = do
  (_, s) <- runStateT game (GameState (Scores 0 0) [])
  print . scores $ s
