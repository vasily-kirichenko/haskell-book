module Readers where

import           Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupledd :: String -> (String, String)
tupledd = do
  x <- cap
  y <- rev
  return (x, y)

tupledb :: String -> (String, String)
tupledb =
  cap >>= \x ->
  rev >>= \y ->
  return (x, y)
