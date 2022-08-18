module Main (main) where

import qualified Data.Solid.ProtoTest
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  t <- testGroup "Array" <$> tests
  defaultMain t

tests :: IO [TestTree]
tests = do
  tests <-
    traverse
      testSpecs
      [ Data.Solid.ProtoTest.tests
      ]
  pure (concat tests)
