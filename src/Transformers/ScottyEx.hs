{-# LANGUAGE OverloadedStrings #-}

module ScottyEx where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           Network.Wreq               (Response)
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config =
  Config {
    count  :: IORef (M.Map Text Integer)
  , prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let v =
        case M.lookup k m of
          Just v -> v + 1
          Nothing -> 1
  in (M.insert k v m, v)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    let key' :: String
        key' = mappend undefined unprefixed
    newInteger <- undefined
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config =
        Config {
          count = counter
        , prefix = TL.pack prefixArg }
      runR :: ReaderT Config IO Response -> IO Response
      runR = ReaderT . const
  scottyT 3000 runR app
