{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tensor.Shape
  ( Dims
  , Shape(..)
  )
where

import Data.Binary
import Data.Positive
import Data.Text.Prettyprint.Doc
import GHC.Generics              (Generic)

type Dims = [Positive]

-- | Shape, potentially annotated with a batch size.
--   Shapes are reversed; i.e. the fastest changing element comes first.
data Shape = Shape
  { shDims  :: Dims
  , shBatch :: Maybe Positive
  }
  deriving (Eq, Show, Generic)

instance Binary Shape

instance Pretty Shape where
  pretty (Shape dims b) = encloseSep mempty mempty "×" (prettyBatch b : fmap pretty (reverse dims))
    where
    prettyBatch :: Maybe Positive -> Doc ann
    prettyBatch Nothing  = "?"
    prettyBatch (Just b) = pretty b
