{-# LANGUAGE DeriveGeneric #-}

module Data.Shape
  ( Dims
  , Shape(..)
  , dimsSize
  , unifyShapes
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

-- | Find, if possible, the most general shape that matches both arguments.
unifyShapes
  :: Shape
  -> Shape
  -> Either String Shape
unifyShapes sa@(Shape da ba) sb@(Shape db bb) = Shape da <$>
  case (ba,bb) of
    _ | da /= db -> Left $ unwords
        [ "Cannot unify shapes ", show sa , "and", show sb <> "; different dimensions" ]
    (Just b,Just b')
      | b /= b' -> Left "Batches differ"
      | otherwise -> pure $ Just b
    (Nothing,b) -> pure b
    (b,Nothing) -> pure b
