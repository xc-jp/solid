module Data.Solid.Lens where

import Data.Solid.Common (Tensor (Tensor))
import Data.Solid.Shape (Dims)
import Lens.Micro (Lens, Lens')

tensorDims :: Lens' (Tensor v a) Dims
tensorDims f (Tensor ds v) = flip Tensor v <$> f ds

-- | Change the base of the tensor.
-- This is unsafe since we don't check if the result has the same number of elements
tensorData :: Lens (Tensor v a) (Tensor w b) (v a) (w b)
tensorData f (Tensor sh v) = Tensor sh <$> f v
