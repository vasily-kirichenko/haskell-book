{-# LANGUAGE NamedFieldPuns #-}

module Queues where

import           Criterion.Main

data Queue a =
  Queue {
    enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue e d) = Queue (a:e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue e (x:xs)) = Just (x, Queue e xs)
pop (Queue e []) =
  case reverse e of
    [] -> Nothing
    h:t -> Just (h, Queue [] t)

planeList :: Int -> [Int]
planeList n = go [1..n]
  where go [] = []
        go xs = go . removeLast [] $ xs
        removeLast _ [] = []
        removeLast acc [_] = acc
        removeLast acc (x:xs) = removeLast (x:acc) xs

queue :: Int -> [Int]
queue n = dequeue . enqueue n $ Queue [] []
  where enqueue 0 q = q
        enqueue n' q = enqueue (n'-1) (push n' q)
        dequeue q =
          case pop q of
            Nothing -> []
            Just (_, queue') -> dequeue queue'

main :: IO ()
main = defaultMain
  [ bench "list" $ whnf planeList 1000
  , bench "queue" $ whnf queue 1000
  ]
