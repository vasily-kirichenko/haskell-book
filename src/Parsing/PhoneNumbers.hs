module PhoneNumbers where

import           Data.Char     (ord)

import           Control.Monad

import           Data.Monoid
import           Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

skip :: Parser ()
skip = skipMany $ oneOf "-() "

dig :: Parser Int
dig = do
  c <- skip *> digit
  return $ ord c - 48

nDigitsInt :: Int -> Parser Int
nDigitsInt n =
  getSum <$> foldMap
    (\pos -> do
        d <- dig
        return . Sum $ (10 ^ (n - getSum pos)) * d)
    (map Sum [1..n])

parsePhone :: Parser PhoneNumber
parsePhone = do
  area <- nDigitsInt 3
  exchange <- nDigitsInt 3
  number <- nDigitsInt 4
  return $ PhoneNumber area exchange number
