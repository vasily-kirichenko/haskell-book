{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

headerEx = "[blah]" :: ByteString

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  value <- some (noneOf "\n")
  skipEOL
  return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

testS = "key=value\nblah=123" :: ByteString
commentEx = "; comment 1" :: ByteString
commenctEx' = "; blah\n; woot\n \n;hah" :: ByteString

skipComment :: Parser ()
skipComment =
  skipMany (do char ';' <|> char '#'
               skipMany $ noneOf "\n"
               skipEOL)

sectionEx = "; ignore me\n[states]\nKot=Moscow" :: ByteString

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Kot=Moscow
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments deriving (Eq, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComment
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections
