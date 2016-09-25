module Parsers where

import           Control.Applicative
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

stringParser' :: Parser String
stringParser' =
  traverse char "123"
  <|> traverse char "12"
  <|> traverse char "1"
