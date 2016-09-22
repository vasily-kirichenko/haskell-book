module HttpStuff where

import           Data.ByteString.Lazy hiding (map)
import           Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.com/bytes/5"
       ]

mappingGet :: IO [Response ByteString]
mappingGet = traverse get urls
