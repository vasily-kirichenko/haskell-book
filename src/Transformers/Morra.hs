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

getComputerGuess :: Int -> StateT Scores IO Int
getComputerGuess maxNumber = liftIO $ randomRIO (0, maxNumber)

askPlayerForNumber :: String -> IO Int
askPlayerForNumber caption =
  putStr (caption ++ ": ") >> readLn :: IO Int

game :: StateT Scores IO ()
game = do
  playerGuess <- liftIO $ askPlayerForNumber "\nYour guess"
  computerGuess <- getComputerGuess 10
  liftIO . putStrLn $ "Computer guess: " ++ show computerGuess
  playerFingers <- liftIO $ askPlayerForNumber "How many fingers? "
  computerFingers <- getComputerGuess 5
  let totalFingers = playerFingers + computerFingers
  scores <- get
  newScores <-
    if totalFingers == playerGuess then do
      liftIO $ putStrLn "Your guess was right!"
      return $ Scores (player scores + 1) (computer scores)
    else if totalFingers == computerGuess then do
      liftIO $ putStrLn "Computer guess was right, sorry."
      return $ Scores (player scores) (computer scores + 1)
    else do
      liftIO $ putStrLn "Nobody guessed."
      return scores

  put newScores
  liftIO . putStrLn $ "Current score: " ++ show newScores

  if player newScores == 3 then
    liftIO $ putStrLn "You win!"
  else if computer newScores == 3 then
    liftIO $ putStrLn "Computer wins, sorry."
  else game

main :: IO ()
main = do
  (_, scores) <- runStateT game (Scores 0 0)
  print scores
