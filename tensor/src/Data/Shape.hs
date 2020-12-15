{-# LANGUAGE DeriveGeneric #-}

module Data.Shape
  ( Dims
  , Shape(..)
  , dimsSize
  , unifyShapes
  , unifyDims
  )
where

import           Control.Monad             (guard)
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

-- | Does the `Dims` satisfy the constraints imposed by the `Shape`? If so,
-- return the expected `Dims` adapted to fit the structure of the `Shape`.
unifyDims :: Shape -> Dims -> Either String Dims
unifyDims sh@(Shape ds batch) dims
  | ds == dims = pure dims
  | ds == ds' = do
    unifyBatch batch b' -- compare batch sizes
    pure (ds <> [b'])
  | otherwise = Left $ unwords
    [ "Cannot unify dimensions"
    , show dims
    , "with shape"
    , show sh
    ]
    where
      ds' = take (length ds) dims
      -- Multiply remaining dimensions into the "batch size" expected by the
      -- shape
      b' = product $ drop (length ds) dims
      unifyBatch Nothing _  = pure ()
      unifyBatch (Just a) x = guard $ a == x
