{-# LANGUAGE QuasiQuotes #-}

module LogFiles where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe          (fromMaybe)
import           Data.Time           hiding (parseTime)
import           Text.RawString.QQ

import           Data.List
import           Text.Trifecta

data Log = Log [LogDay] deriving Eq
data LogDay = LogDay Day [Activity] deriving Eq
type ActivityName = String
data Activity = Activity TimeOfDay ActivityName deriving Eq

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

skipEOL :: Parser ()
skipEOL = skipMany $ char '\n'

skipComment :: Parser ()
skipComment = do
  whiteSpace
  _ <- char '-'
  _ <- char '-'
  skipMany (noneOf "\n")
  skipEOL

parseActivity :: Parser Activity
parseActivity = do
  time <- parseTime
  whiteSpace
  name <- many (noneOf "\n") --`manyTill` (skipComment <|> skipEOL)
  skipEOL
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
  skipMany (noneOf "\n")
  skipEOL
  return $ fromGregorian year month day

parseLogDay :: Parser LogDay
parseLogDay = LogDay <$> parseDay <*> some parseActivity

parseLog :: Parser Log
parseLog = Log <$> many (whiteSpace *> parseLogDay)

toMaybe :: Result a -> Maybe a
toMaybe (Success a) = Just a
toMaybe (Failure _) = Nothing

timeByActivity :: Log -> [(ActivityName, DiffTime)]
timeByActivity (Log days) =
    let activities =
          concatMap (\(LogDay _ activities) -> activities) days
        groupped =
          groupBy
            (\(Activity _ n1) (Activity _ n2) -> n1 == n2)
            activities
        timeByName =
          map
            (\as ->
              let (Activity _ name) = head as
                  totalTime =
                    sum .
                    map (\(Activity t _) -> timeOfDayToTime t) $
                    as
              in (name, totalTime)
            )
            groupped
    in timeByName

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
