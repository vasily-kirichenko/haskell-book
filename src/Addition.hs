module Addition where

import           Test.QuickCheck

myMul :: (Ord a, Eq a, Num a) => a -> a -> a
myMul n = go 0
  where
    go acc p
      | p <= 0 = acc
      | otherwise = go (acc + n) (p - 1)

additionalGreater :: Int -> Bool
additionalGreater x = x + 1 < x

main :: IO ()
main = do
  verboseCheck additionalGreater
