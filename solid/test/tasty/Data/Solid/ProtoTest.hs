module Data.Solid.ProtoTest (tests) where

import Data.Solid.Vector
import Test.Hspec (Spec, describe, it, shouldBe)

tests :: Spec
tests =
  describe "Data.Solid.Vector" $ do
    describe "fromList" $ do
      it "fails when passing fewer elements than declared on the `Dims`" $
        fromList [5, 2] [1 .. 4] `shouldBe` (Nothing :: Maybe (UArray Float))
      it "creates an array when passing strictly more elements than declared on the `Dims`" $
        let Just val = fromList [3, 2] [1 .. 7] :: Maybe (UArray Float)
            Just target = fromList [3, 2] [1 .. 6]
         in val `shouldBe` target
      it "creates an array when passing exactly the number of elements declared on the `Dims`" $
        let Just val = fromList [2, 2] [1 .. 4] :: Maybe (UArray Float)
            Just target = fromList [2, 2] [1, 2, 3, 4]
         in val `shouldBe` target
    describe "fill" $
      it "preserves dimensions" $
        let val = fill [3, 2] pi :: UArray Float
            Just target = fromList [3, 2] [pi, pi, pi, pi, pi, pi]
         in val `shouldBe` target
