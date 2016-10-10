module Chipher where

import           Control.Arrow
import           Data.Char
import           Data.List

type Keyword = String

charPairs :: Keyword -> String -> [(Char, Char)]
charPairs k s =
  reverse . snd $
  foldl'
    (\ (k, acc) s ->
      if s == ' ' then (k, (' ', ' ') : acc)
      else
        let (kh : kt) = k
        in (kt, (kh, s) : acc)
    )
    (concat . repeat $ k, [])
    s

ordPairs :: Keyword -> String -> [(Int, Int)]
ordPairs k s = map ((\k -> ord k - ord 'A') *** ord) $ charPairs k s

encode :: Keyword -> String -> String
encode k s =
  let shifted =
        map
          (\(k, s) ->
            let n = if s == 32 then 32 else s + k in
            if n > ord 'Z' then n - (ord 'Z' - ord 'A') else n)
          (ordPairs k s)
  in map chr shifted
  --in shifted

main :: IO ()
main = print $ encode "foo" "bar"
