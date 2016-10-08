module Main where

import qualified Data.Map        as M
import           Morse
import           Test.Hspec
import           Test.QuickCheck

allowedChars :: String
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain

testTwoPlusTwo =
  describe "foo" $
    it "2 + 2 should be 4" $
      2 + 2 `shouldBe` 4
