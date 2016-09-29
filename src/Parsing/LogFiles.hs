{-# LANGUAGE QuasiQuotes #-}

module LogFiles where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe          (fromMaybe)
import           Data.Time           hiding (parseTime)
import           Text.RawString.QQ
import           Text.Trifecta

data Log = Log [LogDay] deriving (Eq, Show)
data LogDay = LogDay Day [Activity] deriving (Eq, Show)
type Name = String
data Activity = Activity TimeOfDay Name deriving (Eq, Show)

parseTime :: Parser TimeOfDay
parseTime = do
  hours <- fromInteger <$> integer
  char ':'
  minutes <- fromInteger <$> integer
  case makeTimeOfDayValid hours minutes 0.0 of
    Nothing -> fail "Wrong time"
    Just x -> return x

eol :: Parser ()
eol = void $ char '\n'

parseActivity :: Parser Activity
parseActivity = do
  time <- parseTime
  whiteSpace
  name <- some $ noneOf "\n"
  skipMany $ noneOf "\n"
  eol
  return $ Activity time name

parseDay :: Parser Day
parseDay = do
  char '#'
  whiteSpace
  year <- integer
  _ <- char '-'
  month <- fromInteger <$> integer
  _ <- char '-'
  day <- fromInteger <$> integer
  skipMany $ noneOf "\n"
  eol
  return $ fromGregorian year month day

parseLogDay :: Parser LogDay
parseLogDay = LogDay <$> parseDay <*> some parseActivity

parseLog :: Parser Log
parseLog = Log <$> many (whiteSpace *> parseLogDay)

logDay :: String
logDay = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
