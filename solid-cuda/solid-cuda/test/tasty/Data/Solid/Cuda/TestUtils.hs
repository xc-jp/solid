{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Solid.Cuda.TestUtils
  ( testCudaTWith,
    propertyCudaT,
    eqCudaT,
    arrayEqCudaT,
    expectAnyExceptionCudaT,
    expectExceptionCudaT,
    DimsSized (..),
    SArraySized (..),
  )
where

import Control.Monad
import Data.Solid.Approx
import Data.Solid.Array
import Data.Solid.Cuda
import Data.Solid.Shape
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as V
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- * Cuda interaction

testCudaTWith :: Testable p => (Either CudaException a -> p) -> CudaT IO a -> Property
testCudaTWith f cudaAction = monadicIO $ f <$> run (runCudaT cudaAction)

-- | test the property when there is no exception
propertyCudaT :: Testable t => CudaT IO t -> Property
propertyCudaT = testCudaTWith $ \case
  Left e -> unexpectedFailure e
  Right a -> property a

eqCudaT :: (Eq a, Show a) => a -> CudaT IO a -> Property
eqCudaT expected = propertyCudaT . fmap (expected ===)

arrayEqCudaT ::
  (VG.Vector f a, Eq (AApproxElt f a), Show (f a)) =>
  Array f a ->
  CudaT IO (Array f a) ->
  Property
arrayEqCudaT expected actual =
  tapproxElt expected `eqCudaT` (tapproxElt <$> actual)

expectAnyExceptionCudaT :: CudaT IO a -> Property
expectAnyExceptionCudaT = testCudaTWith $ \case
  Left _ -> property True
  Right _ -> unexpectedSuccess

expectExceptionCudaT :: CudaException -> CudaT IO a -> Property
expectExceptionCudaT e = testCudaTWith $ \case
  Left e' -> e === e'
  Right _ -> unexpectedSuccess

-- * error messages

unexpectedFailure :: CudaException -> Property
unexpectedFailure e = counterexample ("Unexpected exception when running CUDA: " <> show e) $ property False

unexpectedSuccess :: Property
unexpectedSuccess = counterexample "Expecting exceptions but got success when running CUDA" $ property False

-- * generators

newtype DimsSized = DimsSized Dims
  deriving (Eq, Show)

instance Arbitrary DimsSized where
  arbitrary = sized $ \n -> do
    nIndices <- choose (1, 4)
    ds <- replicateM nIndices $ choose (1, max 1 n)
    pure . DimsSized . (fromIntegral <$>) $ ds

newtype SArraySized a = SArraySized (SArray a)
  deriving (Eq, Show)

instance (Storable a, Arbitrary a) => Arbitrary (SArraySized a) where
  arbitrary = do
    DimsSized dims <- resize 10 arbitrary
    imgs <- vector $ dimsSize dims
    pure . SArraySized . Array dims $ V.fromList imgs
