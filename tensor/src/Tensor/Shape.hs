{-# LANGUAGE DeriveGeneric #-}

module Tensor.Shape
  ( Dims
  , Shape(..)
  )
where

import Data.Binary
import Data.Positive
import GHC.Generics  (Generic)

type Dims = [Positive]

-- | Shape, potentially annotated with a batch size.
--   Shapes are reversed; i.e. the fastest changing element comes first.
data Shape = Shape
  { shDims  :: Dims
  , shBatch :: Maybe Positive
  }
  deriving (Eq, Show, Generic)

instance Binary Shape
