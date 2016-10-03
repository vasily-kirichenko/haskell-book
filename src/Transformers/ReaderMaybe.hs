module ReaderMaybe where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

getx :: ReaderT String Maybe Int
getx = ReaderT $ \s -> Just . read $ s

addTwo :: String -> Maybe Int
addTwo = runReaderT $ do
  x <- getx
  y <- lift $ Just 2
  return $ x + y

getx' :: MaybeT (Reader String) Int
getx' = MaybeT (ReaderT $ \s -> read s)

-- addTwo' :: String -> Maybe Int
-- addTwo' = runReader . runMaybeT $ do
--   x <- getx'
--   y <- lift $ (ReaderT $ \s -> 2)
--   return $ x + y
