module Exceptions where


import           System.Environment

import           Control.Exception

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

willFail :: Integer -> IO ()
willFail = onlyReportError . willIFail

willFail' :: Integer -> IO ()
willFail' denom =
  print (div 5 denom) `catch` handler
    where handler :: ArithException -> IO ()
          handler = print

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willIFail (read d)

main :: IO ()
main = do
  args <- getArgs
  mapM_ testDiv args

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch = try . throwIO
