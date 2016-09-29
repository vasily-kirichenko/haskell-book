{-# LANGUAGE QuasiQuotes #-}

module LogFiles where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe          (fromMaybe)
import           Data.Time           hiding (parseTime)
import           Text.RawString.QQ
import           Text.Trifecta

data Log = Log [LogDay] deriving Eq
data LogDay = LogDay Day [Activity] deriving Eq
type Name = String
data Activity = Activity TimeOfDay Name deriving Eq

instance Show Activity where
  show (Activity time name) = show time ++ " " ++ name

instance Show LogDay where
  show (LogDay day activities) =
    "\n# " ++ show day ++ "\n" ++
    foldr (\a r -> show a ++ "\n" ++ r) "" activities

instance Show Log where
  show (Log logDays) =
    foldr (\x r -> show x ++ "\n" ++ r) "" logDays

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
  char '-'
  month <- fromInteger <$> integer
  char '-'
  day <- fromInteger <$> integer
  skipMany $ noneOf "\n"
  eol
  return $ fromGregorian year month day

parseLogDay :: Parser LogDay
parseLogDay = LogDay <$> parseDay <*> some parseActivity

parseLog :: Parser Log
parseLog = Log <$> many (whiteSpace *> parseLogDay)

logEx :: String
logEx = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

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
