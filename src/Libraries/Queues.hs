{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Queues where

import           Criterion.Main

data Queue a =
  Queue {
    enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a q@Queue{enqueue} = q { enqueue = a : enqueue }

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue e (x:xs)) = Just (x, Queue e xs)
pop (Queue xs []) =
  let (h:t) = reverse xs
  in Just (h, Queue [] t)

planeList :: Int -> [Int]
planeList n = go [1..n]
  where go [] = []
        go xs = go . removeLast [] $ xs
        removeLast _ [] = []
        removeLast acc [x] = acc
        removeLast acc (x:xs) = removeLast (x:acc) xs

queue :: Int -> [Int]
queue n = de (en n $ Queue [] [])
  where en 0 q = q
        en n q = en (n-1) (push n q)
        de q =
          case pop q of
            Nothing -> []
            Just (_, q) -> de q

main :: IO ()
main = defaultMain
  [ bench "list" $ whnf planeList 1000
  , bench "queue" $ whnf queue 1000
  ]
