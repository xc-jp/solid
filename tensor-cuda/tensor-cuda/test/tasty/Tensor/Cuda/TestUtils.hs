{-# LANGUAGE LambdaCase #-}

module Tensor.Cuda.TestUtils where

import Control.Monad (replicateM, (<=<))
import Data.Shape
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Tensor.Common (Tensor (..))
import Tensor.Cuda
import Tensor.Vector (STensor)
import Test.QuickCheck hiding (Positive)
import Test.QuickCheck.Monadic
  ( PropertyM,
    assert,
    monadicIO,
    run,
  )
import Test.Tasty.QuickCheck (Property)

-- * Cuda interaction

testCudaTWith :: (Either CudaException a -> PropertyM IO ()) -> CudaT IO a -> Property
testCudaTWith f = monadicIO . (f <=< run) . runCudaT

assertCudaT :: CudaT IO Bool -> Property
assertCudaT = testCudaTWith $ \case
  Left _ -> assert False
  Right b -> assert b

-- * generators

newtype DimsSized = DimsSized Dims
  deriving (Eq, Show)

instance Arbitrary DimsSized where
  arbitrary = sized $ \n -> do
    nIndices <- choose (1, 4)
    ds <- replicateM nIndices $ choose (1, max 1 n)
    pure . DimsSized . (fromIntegral <$>) $ ds

newtype STensorSized a = STensorSized (STensor a)
  deriving (Eq, Show)

instance (Storable a, Arbitrary a) => Arbitrary (STensorSized a) where
  arbitrary = do
    DimsSized dims <- resize 10 arbitrary
    imgs <- vector $ dimsSize dims
    pure . STensorSized . Tensor dims $ V.fromList imgs
