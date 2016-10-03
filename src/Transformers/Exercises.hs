module Exercises where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Control.Monad.Trans.State
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  liftIO $ putStr "Hi: " >> print r
  return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  liftIO $ putStr "Hi: " >> print s
  return (show s, s + 1)
