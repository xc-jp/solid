{-# LANGUAGE DeriveGeneric #-}

module Data.Shape
  ( Dims
  , Shape(..)
  , dimsSize
  )
where

import           Data.Binary
import           Data.Positive
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)

type Dims = [Positive]

dimsSize :: Dims -> Int
dimsSize = product . fmap fromIntegral

-- | Shape, potentially annotated with a batch size.
--   Shapes are reversed; i.e. the fastest changing element comes first.
data Shape = Shape
  { shDims  :: Dims
  , shBatch :: Maybe Positive
  }
  deriving (Eq, Show, Generic)

instance Binary Shape

instance Pretty Shape where
  pretty (Shape dims b) = concatWith (\x y -> x <> pretty "×" <> y) (prettyBatch b : fmap pretty (reverse dims))
    where
    prettyBatch :: Maybe Positive -> Doc ann
    prettyBatch Nothing  = pretty "?"
    prettyBatch (Just b) = pretty b
