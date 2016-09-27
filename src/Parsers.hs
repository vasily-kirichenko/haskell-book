{-# LANGUAGE DatatypeContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Parsers where

import           Control.Applicative
import           Control.Monad       (forM_)
import           Data.Ratio          ((%))
import           Text.RawString.QQ
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

parseRational :: Parser Rational
parseRational = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseFractional :: Fractional a => Parser a
parseFractional = do
  firstPart <- decimal
  char '.'
  secondPart <- decimal
  let secondPartCount = numberCount secondPart
  return $
    fromInteger (firstPart * (10 ^ secondPartCount) + secondPart) /
    (10 ^ secondPartCount)
  where
    numberCount = go 0
    go count n =
      case n `divMod` 10 of
        (0, _) -> count + 1
        (x, _) -> go (count + 1) x

data Fractional a => RationalOrFractional a
  = Rat Rational
  | Frac a
  deriving (Eq, Show)

rationalOrFractional :: Fractional a => Parser (RationalOrFractional a)
rationalOrFractional =
  try (Rat <$> parseRational) <|> (Frac <$> parseFractional)

myParse :: Parser Integer
myParse = integer <* eof

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

main :: IO ()
main =
  print $ parseString (some (token parseNos)) mempty eitherOr
