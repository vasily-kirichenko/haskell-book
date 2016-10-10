{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Queues where

import           Criterion.Main

data Queue a =
  Queue
    { enqueue :: [a]
    , dequeue :: [a]
    } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a q@Queue{enqueue} = q { enqueue = a : enqueue }

pop :: Queue a -> Maybe (a, Queue a)
pop q@Queue{..} =
  case dequeue of
    x:xs -> Just (x, q { dequeue = xs })
    [] ->
      case enqueue of
        [] -> Nothing
        xs ->
          let (x:xs) = reverse xs
          in Just (x, q { dequeue = xs, enqueue = [] })

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
  , bench "queue" $ whnf queue 1000 ]
