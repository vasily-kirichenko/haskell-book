module SemanticVersion where

import           Control.Applicative
import           Data.Maybe          (maybeToList)
import           Text.Trifecta

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseNOS :: Parser NumberOrString
parseNOS = (NOSS <$> some letter) <|> (NOSI <$> integer)

parseExtra :: Parser [NumberOrString]
parseExtra = many (parseNOS <* skipMany dot)

parseSevVer :: Parser SemVer
parseSevVer = do
 major <- integer <* dot
 minor <- integer <* dot
 patch <- integer
 release <- optional $ char '-' *> parseExtra
 metadata <- optional $ char '+' *> parseExtra
 return $
   SemVer major minor patch
     (concat . maybeToList $ release)
     (concat . maybeToList $ metadata)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 _ _)
          (SemVer major2 minor2 patch2 _ _) =
    case major1 `compare` major2 of
      EQ ->
        case minor1 `compare` minor2 of
          EQ -> patch1 `compare` patch2
          x -> x
      x -> x
