module Tensor.ProtoTest (tests) where

import Data.Solid.Vector
import Test.Tasty.Hspec

tests :: Spec
tests =
  describe "Tensor.Vector" $ do
    describe "fromList" $ do
      it "fails when passing fewer elements than declared on the `Dims`" $
        fromList [5, 2] [1 .. 4] `shouldBe` (Nothing :: Maybe (UTensor Float))
      it "creates a tensor when passing strictly more elements than declared on the `Dims`" $
        let Just val = fromList [3, 2] [1 .. 7] :: Maybe (UTensor Float)
            Just target = fromList [3, 2] [1 .. 6]
         in val `shouldBe` target
      it "creates a tensor when passing exactly the number of elements declared on the `Dims`" $
        let Just val = fromList [2, 2] [1 .. 4] :: Maybe (UTensor Float)
            Just target = fromList [2, 2] [1, 2, 3, 4]
         in val `shouldBe` target
    describe "fill" $
      it "preserves dimensions" $
        let val = fill [3, 2] pi :: UTensor Float
            Just target = fromList [3, 2] [pi, pi, pi, pi, pi, pi]
         in val `shouldBe` target
