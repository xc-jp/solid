module Main (main) where

import qualified Data.Solid.Cuda.MemoryTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ Data.Solid.Cuda.MemoryTest.tests
    ]
