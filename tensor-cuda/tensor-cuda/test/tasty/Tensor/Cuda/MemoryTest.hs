module Tensor.Cuda.MemoryTest (tests) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.Storable (Storable)
import Tensor
import Tensor.Cuda
import Tensor.Cuda.TestUtils
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
      testProperty "identity (STensor Int)" $
        \(STensorSized as) -> prop_readAfterWriteIdentityTensor (as :: STensor Int),
      testProperty "identity (STensor Float)" $
        \(STensorSized as) -> prop_readAfterWriteIdentityTensor (as :: STensor Float),
      testProperty "identity (STensor CInt)" $
        \(STensorSized as) -> prop_readAfterWriteIdentityTensor (as :: STensor CInt),
      testProperty "identity (STensor CFloat)" $
        \(STensorSized as) -> prop_readAfterWriteIdentityTensor (as :: STensor CFloat)
    ]

-- \forall Storable a. withCuda a peekCuda == pure a
prop_readAfterWriteIdentity :: (Eq a, Storable a, Show a) => a -> Property
prop_readAfterWriteIdentity a = a `eqCudaT` withCuda a peekCuda

-- \forall Storable a, as \in Vector a. withCudaVector as peekCudaVector == pure as
prop_readAfterWriteIdentityVector :: (Eq a, Storable a, Show a) => Vector a -> Property
prop_readAfterWriteIdentityVector as = as `eqCudaT` withCudaVector as peekCudaVector

-- \forall Storable a, as \in STensor a. withCudaTensor as peekCudaTensor == pure as
prop_readAfterWriteIdentityTensor :: (Eq a, Storable a, Show a) => STensor a -> Property
prop_readAfterWriteIdentityTensor as = as `eqCudaT` withCudaTensor as peekCudaTensor
