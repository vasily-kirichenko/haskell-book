{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import           Control.Applicative
import           Control.Monad       (forM_)
import           Data.Ratio          ((%))
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

one'' :: Parser Char
one'' = char '1' <* eof

oneTwo'' :: Parser Char
oneTwo'' = oneTwo <* eof

stringParser :: Parser String
stringParser = string "123" <|> string "12" <|> string "1"

-- does not work yet
stringParser' :: Parser String
stringParser' =
  traverse char "123"
  <|> traverse char "12"
  <|> traverse char "1"

-- fractions

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

myParse :: Parser Integer
myParse = integer <* eof

main :: IO ()
main = do
  forM_
    [shouldWork, shouldAlsoWork, alsoBad, badFraction]
    (print . parseString parseFraction mempty)
