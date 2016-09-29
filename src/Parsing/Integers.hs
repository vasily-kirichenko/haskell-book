module Integers where

import           Data.Char     (ord)
import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "Digit"

base10Integer :: Parser Integer
base10Integer = do
  chars <- some parseDigit
  let digits = map (toInteger . (\x -> x - 48) . ord) chars
  return . snd $
    foldr
      (\d (coeff, res) -> (coeff + 1, res + (10 ^ coeff) * d))
      (0, 0)
      digits
