module Tensor.Shape where

type Dims = [Int]

-- | Shape, potentially annotated with a batch size.
--   Shapes are reversed; i.e. the fastest changing element comes first.
data Shape = Shape
  { shDims  :: Dims
  , shBatch :: Maybe Int
  }
  deriving (Eq, Show)
