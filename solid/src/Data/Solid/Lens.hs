module Data.Solid.Lens where

import Data.Solid.Common (Array (Array))
import Data.Solid.Shape (Dims)
import Lens.Micro (Lens, Lens')

arrayDims :: Lens' (Array v a) Dims
arrayDims f (Array ds v) = flip Array v <$> f ds

-- | Change the base of the array.
-- This is unsafe since we don't check if the result has the same number of elements
arrayData :: Lens (Array v a) (Array w b) (v a) (w b)
arrayData f (Array sh v) = Array sh <$> f v
