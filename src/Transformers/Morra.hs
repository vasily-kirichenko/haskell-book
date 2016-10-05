module Morra where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.List                 as L
import           Data.Maybe                (fromMaybe)
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

getPreviousPlayerGuess :: [Int] -> Maybe Int
getPreviousPlayerGuess guesses =
  case reverse . fst . L.splitAt 2 $ guesses of
    [x, y] ->
      let guesses' = reverse . drop 1 $ guesses in
      do i <- L.elemIndex x guesses'
         case drop i guesses' of
           _ : y' : z : _
            | y == y' -> Just z
            | otherwise -> Nothing
           _ -> Nothing
    _ -> Nothing

getComputerGuess :: Int -> StateT GameState IO Int
getComputerGuess maxNumber = do
  playerGuesses <- allPlayerGuesses <$> get
  case getPreviousPlayerGuess playerGuesses of
    Just guess -> return guess
    Nothing -> liftIO $ randomRIO (0, maxNumber)

getComputerFingers :: IO Int
getComputerFingers = randomRIO (0, 5)

askPlayerForGuess :: StateT GameState IO Int
askPlayerForGuess = do
   liftIO $ putStr "\nYour guess: "
   guess <- liftIO (readLn :: IO Int)
   state <- get
   put $ state { allPlayerGuesses = guess : allPlayerGuesses state }
   return guess

askPlayerForNumber :: String -> IO Int
askPlayerForNumber caption =
  putStr (caption ++ ": ") >> readLn :: IO Int

writeln :: MonadIO m => String -> StateT a m ()
writeln = liftIO . putStrLn

game :: StateT GameState IO ()
game = do
  playerGuess <- askPlayerForGuess

  computerGuess <- getComputerGuess 10
  writeln $ "Computer guess: " ++ show computerGuess

  playerFingers <- liftIO $ askPlayerForNumber "How many fingers? "

  computerFingers <- getComputerGuess 5
  writeln $ "Computer showed " ++ show computerFingers ++ " fingers."

  let totalFingers = playerFingers + computerFingers
  writeln $ "Total fingers = " ++ show totalFingers

  state <- get
  let oldScores = scores state

  newScores <-
    if totalFingers == playerFingers && totalFingers == computerFingers then do
      writeln "Both guess right!"
      return $ Scores (player oldScores + 1) (computer oldScores + 1)
    else if totalFingers == playerGuess then do
      writeln "Your guess was right!"
      return $ oldScores { player = player oldScores + 1 }
    else if totalFingers == computerGuess then do
      writeln "Computer guess was right, sorry."
      return $ oldScores { computer = computer oldScores + 1 }
    else do
      writeln "Nobody guessed."
      return oldScores

  let newState = state { scores = newScores }
  writeln $ "Current state: " ++ show newState
  put newState

  case (player newScores, computer newScores) of
    (3, _) -> writeln "You win!"
    (_, 3) -> writeln "Computer wins, sorry."
    _ -> game

main :: IO ()
main = do
  (_, s) <- runStateT game (GameState (Scores 0 0) [])
  print . scores $ s
