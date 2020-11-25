module Tensor.ProtoTest (tests) where

import Data.Elt
import Tensor.Common
import Tensor.List
import Test.Tasty.Hspec

tests :: Spec
tests = do
  describe "Tensor.List" $ do
    it "fromList: bigger declared Dims than size of list." $
      fromList [5, 2] [(1 :: Float) .. 4] `shouldBe` Nothing
    it "fromList: lesser declared Dims than size of list." $
      fromList [3, 2] [(1 :: Float) .. 5] `shouldBe` Nothing
    it "fromList: correct Dims for passed list." $
      let val = fromList [2, 2] [(1 :: Float) .. 4]
          target = Tensor [2, 2] EltFloat [(1 :: Float), 2, 3, 4]
       in val `shouldBe` (Just target)
    it "Dimensions preserved on fill" $
      let val = fill [3, 2] (pi :: Double)
          target = Tensor [3, 2] EltDouble [pi, pi, pi, pi, pi, pi]
       in val `shouldBe` target
