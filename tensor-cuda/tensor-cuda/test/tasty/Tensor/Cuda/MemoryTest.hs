module Tensor.Cuda.MemoryTest (tests) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.Storable (Storable)
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
        \as -> prop_readAfterWriteIdentityTensor (as :: STensorSized Int),
      testProperty "identity (STensor Float)" $
        \as -> prop_readAfterWriteIdentityTensor (as :: STensorSized Float),
      testProperty "identity (STensor CInt)" $
        \as -> prop_readAfterWriteIdentityTensor (as :: STensorSized CInt),
      testProperty "identity (STensor CFloat)" $
        \as -> prop_readAfterWriteIdentityTensor (as :: STensorSized CFloat)
    ]

-- \forall Storable a. withCuda a peekCuda == pure a
prop_readAfterWriteIdentity :: (Eq a, Storable a) => a -> Property
prop_readAfterWriteIdentity a = assertCudaT . fmap (== a) $ withCuda a peekCuda

-- \forall Storable a, as \in Vector a. withCudaVector as peekCudaVector == pure as
prop_readAfterWriteIdentityVector :: (Eq a, Storable a) => Vector a -> Property
prop_readAfterWriteIdentityVector as = assertCudaT . withCudaVector as $ \cpas n -> do
  (== as) <$> peekCudaVector cpas n

-- \forall Storable a, as \in STensor a. withCudaTensor as peekCudaTensor == pure as
prop_readAfterWriteIdentityTensor :: (Eq a, Storable a) => STensorSized a -> Property
prop_readAfterWriteIdentityTensor (STensorSized as) =
  assertCudaT . withCudaTensor as $
    fmap (== as) . peekCudaTensor
