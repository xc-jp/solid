module Main (main) where

import qualified Tensor.Cuda.MemoryTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ Tensor.Cuda.MemoryTest.tests
    ]
