{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe)
import           System.Random

type Guess = Int
type Fingers = Int

data Scores =
  Scores {
    player   :: Int
  , computer :: Int
  } deriving Show

data GameState =
  GameState {
    scores           :: Scores
  , allPlayerGuesses :: [Guess]
} deriving Show

getPreviousPlayerGuess :: [Guess] -> Maybe Guess
getPreviousPlayerGuess guesses =
  go pat guesses'
  where
    pat = reverse . take 2 $ guesses
    guesses' = reverse . drop 1 $ guesses
    go [x, y] (x':rest@(y':z:_))
      | x == x' && y == y' = Just z
      | otherwise = go pat (L.dropWhile (/=x) rest)
    go _ _ = Nothing

getComputerGuess :: StateT GameState IO Guess
getComputerGuess = do
  playerGuesses <- allPlayerGuesses <$> get
  case getPreviousPlayerGuess playerGuesses of
    Just guess -> return guess
    Nothing -> liftIO $ randomRIO (0, 10)

getComputerFingers :: IO Fingers
getComputerFingers = randomRIO (0, 5)

askPlayerForGuess :: StateT GameState IO Guess
askPlayerForGuess = do
   liftIO $ putStr "\nYour guess: "
   guess <- liftIO readLn
   state @ GameState { allPlayerGuesses } <- get
   put $ state { allPlayerGuesses = guess : allPlayerGuesses }
   return guess

askPlayerForFingers :: IO Fingers
askPlayerForFingers = putStr "How many fingers? : " >> readLn :: IO Fingers

writeln :: MonadIO m => String -> StateT a m ()
writeln = liftIO . putStrLn

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerGuess <- askPlayerForGuess

  computerGuess <- getComputerGuess
  writeln $ "Computer guess: " ++ show computerGuess

  playerFingers <- liftIO askPlayerForFingers

  computerFingers <- liftIO getComputerFingers
  writeln $ "Computer showed " ++ show computerFingers ++ " fingers."

  let totalFingers = playerFingers + computerFingers
  writeln $ "Total fingers = " ++ show totalFingers

  state @ GameState { scores = scores @ Scores {..} } <- get

  newScores <-
    if | totalFingers == playerFingers && totalFingers == computerFingers -> do
           writeln "Both guess right!"
           return $ Scores (player + 1) (computer + 1)
       | totalFingers == playerGuess -> do
           writeln "Your guess was right!"
           return $ scores { player = player + 1 }
       | totalFingers == computerGuess -> do
           writeln "Computer guess was right, sorry."
           return $ scores { computer = computer + 1 }
       | otherwise -> do
           writeln "Nobody guessed."
           return scores

  let newState = state { scores = newScores }
  writeln $ "Current state: " ++ show newState
  put newState

  case newScores of
    Scores { player = 3 } -> writeln "You win!"
    Scores { computer = 3 } -> writeln "Computer wins, sorry."
    _ -> gameLoop

main :: IO ()
main = do
  (_, s) <- runStateT gameLoop (GameState (Scores 0 0) [])
  print . scores $ s
