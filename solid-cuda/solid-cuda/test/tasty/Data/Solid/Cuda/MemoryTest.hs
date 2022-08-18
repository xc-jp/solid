module Data.Solid.Cuda.MemoryTest (tests) where

import Data.Solid.Array
import Data.Solid.Cuda
import Data.Solid.Cuda.TestUtils
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Cuda tests"
    [ testProperty "identity (Int)" $
        \a -> prop_readAfterWriteIdentity (a :: Int),
      testProperty "identity (Float)" $
        \a -> prop_readAfterWriteIdentity (a :: Float),
      testProperty "identity (CInt)" $
        \a -> prop_readAfterWriteIdentity (a :: CInt),
      testProperty "identity (CFloat)" $
        \a -> prop_readAfterWriteIdentity (a :: CFloat),
      testProperty "identity (Vector Int)" $
        \as -> (not . null $ as) ==> prop_readAfterWriteIdentityVector $ V.fromList (as :: [Int]),
      testProperty "identity (Vector Float)" $
        \as -> (not . null $ as) ==> prop_readAfterWriteIdentityVector $ V.fromList (as :: [Float]),
      testProperty "identity (Vector CInt)" $
        \as -> (not . null $ as) ==> prop_readAfterWriteIdentityVector $ V.fromList (as :: [CInt]),
      testProperty "identity (Vector CFloat)" $
        \as -> (not . null $ as) ==> prop_readAfterWriteIdentityVector $ V.fromList (as :: [CFloat]),
      testProperty "identity (SArray Int)" $
        \(SArraySized as) -> prop_readAfterWriteIdentityArray (as :: SArray Int),
      testProperty "identity (SArray Float)" $
        \(SArraySized as) -> prop_readAfterWriteIdentityArray (as :: SArray Float),
      testProperty "identity (SArray CInt)" $
        \(SArraySized as) -> prop_readAfterWriteIdentityArray (as :: SArray CInt),
      testProperty "identity (SArray CFloat)" $
        \(SArraySized as) -> prop_readAfterWriteIdentityArray (as :: SArray CFloat)
    ]

-- \forall Storable a. withCuda a peekCuda == pure a
prop_readAfterWriteIdentity :: (Eq a, Storable a, Show a) => a -> Property
prop_readAfterWriteIdentity a = a `eqCudaT` withCuda a peekCuda

-- \forall Storable a, as \in Vector a. withCudaVector as peekCudaVector == pure as
prop_readAfterWriteIdentityVector :: (Eq a, Storable a, Show a) => Vector a -> Property
prop_readAfterWriteIdentityVector as = as `eqCudaT` withCudaVector as peekCudaVector

-- \forall Storable a, as \in SArray a. withCudaArray as peekCudaArray == pure as
prop_readAfterWriteIdentityArray :: (Eq a, Storable a, Show a) => SArray a -> Property
prop_readAfterWriteIdentityArray as = as `eqCudaT` withCudaArray as peekCudaArray
