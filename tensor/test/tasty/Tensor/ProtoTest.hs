module Tensor.ProtoTest (tests) where

import Data.Elt
import Tensor.Common
import Tensor.List
import Test.Tasty.Hspec

tests :: Spec
tests = do
  describe "Tensor.List" $ do
    describe "fromList" $ do
      it "fails when passing fewer elements than declared on the `Dims`" $
        fromList [5, 2] [(1 :: Float) .. 4] `shouldBe` Nothing
      it "creates a tensor when passing strictly more elements than declared on the `Dims`" $
        let val = fromList [3, 2] [(1 :: Float) .. 7]
            target = Tensor [3, 2] EltFloat [1 .. 6]
         in val `shouldBe` (Just target)
      it "creates a tensor when passing exactly the number of elements declared on the `Dims`" $
        let val = fromList [2, 2] [(1 :: Float) .. 4]
            target = Tensor [2, 2] EltFloat [(1 :: Float), 2, 3, 4]
         in val `shouldBe` (Just target)
    describe "fill" $ do
      it "preserves dimensions" $
        let val = fill [3, 2] (pi :: Double)
            target = Tensor [3, 2] EltDouble [pi, pi, pi, pi, pi, pi]
         in val `shouldBe` target
