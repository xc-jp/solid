module Main (main) where

import qualified Tensor.ProtoTest
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  t <- testGroup "Tensor" <$> tests
  defaultMain t

tests :: IO [TestTree]
tests = do
  tests <-
    traverse
      testSpecs
      [ Tensor.ProtoTest.tests
      ]
  pure (concat tests)
