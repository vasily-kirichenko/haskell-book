module QCheck where

import           Test.QuickCheck

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
  frequency [ (3, return Fulse)
            , (1, return Frue)]

instance Arbitrary Fool where
  arbitrary = foolGen
