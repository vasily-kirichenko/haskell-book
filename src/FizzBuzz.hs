{-# LANGUAGE ViewPatterns #-}

module FizzBuzz where

import           Control.Monad
import           Control.Monad.Trans.State

data FizzBuzz = FizzBuzz | Fizz | Buzz

fb :: Integer -> Maybe FizzBuzz
fb n | n `mod` 15 == 0 = Just FizzBuzz
     | n `mod` 5 == 0 = Just Fizz
     | n `mod` 3 == 0 = Just Buzz
     | otherwise = Nothing

fizzBuzz :: Integer -> String
fizzBuzz (fb -> Just FizzBuzz) = "FizzBuzz"
fizzBuzz (fb -> Just Fizz) = "Fizz"
fizzBuzz (fb -> Just Buzz) = "Buzz"
fizzBuzz n = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = fizzBuzzList [to, to - 1..from]

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100
