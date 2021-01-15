{-# LANGUAGE DeriveGeneric #-}

module Data.Shape
  ( Dims,
    Shape (..),
    dimsSize,
    unifyShapes,
    unifyDims,
    toDims,
    fromDims,
    splitDims,
    broadcastShapes,
    prefixShape,
    (#:.),
  )
where

import Data.Positive
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)

type Dims = [Positive]

dimsSize :: Dims -> Int
dimsSize = product . fmap fromIntegral

-- | Shape, potentially annotated with a batch size.
--   Shapes are little-endian; i.e. the fastest changing dimension comes first.
data Shape
  = Positive :. Shape
  | Z
  | Any
  deriving (Generic, Eq)

instance Show Shape where
  show (x :. Z) = show x
  show (x :. xs) = show x <> "x" <> show xs
  show Any = "?"
  show Z = "Z" -- scalar

infixr 6 :.

instance Pretty Shape where
  pretty (x :. Z) = pretty x
  pretty (x :. xs) = pretty x <> pretty "×" <> pretty xs
  pretty Any = pretty "?"
  pretty Z = pretty "Z"

-- | Find, if possible, the most general shape that matches both arguments.
unifyShapes :: Shape -> Shape -> Maybe Shape
unifyShapes Z Z = pure Z
unifyShapes Any ys = pure ys
unifyShapes xs Any = pure xs
unifyShapes Z (_ :. _) = Nothing
unifyShapes (_ :. _) Z = Nothing
unifyShapes (x :. xs) (y :. ys)
  | x == y = fmap (x :.) (unifyShapes xs ys)
  | otherwise = Nothing

-- | Split a 'Shape' at an index into the 'Dims' up until that index and the
-- remaining 'Shape' from that index. If less dims than the given index are
-- known this fails with 'Nothing'.
splitDims :: Int -> Shape -> Maybe (Dims, Shape)
splitDims 0 z = pure ([], z)
splitDims n (x :. xs)
  | n >= 1 = do
    (ds, z) <- splitDims (pred n) xs
    pure (x : ds, z)
  | otherwise = Nothing
splitDims _ _ = Nothing

-- | Broadcast shapes together.
--
-- We borrow broadcasting semantics from numpy.
-- https://numpy.org/doc/stable/user/basics.broadcasting.html#general-broadcasting-rules
--
-- The broadcasting is bi-directional and the resulting shape has the maximum
-- rank of the two arguments.
broadcastShapes :: Shape -> Shape -> Maybe Shape
broadcastShapes Z ys = pure ys
broadcastShapes xs Z = pure xs
broadcastShapes Any ys = pure ys
broadcastShapes xs Any = pure xs
broadcastShapes (1 :. xs) (y :. ys) = (y :.) <$> broadcastShapes xs ys
broadcastShapes (x :. xs) (1 :. ys) = (x :.) <$> broadcastShapes xs ys
broadcastShapes (x :. xs) (y :. ys)
  | x == y = (x :.) <$> broadcastShapes xs ys
  | otherwise = Nothing

-- | Convert 'Shape' to 'Dims' by replacing batch 'Any' with trailing 'Dims'.
toDims :: Dims -> Shape -> Dims
toDims _ Z = []
toDims z Any = z
toDims z (x :. xs) = x : toDims z xs

-- | Convert 'Dims' to known size 'Shape'
fromDims :: Dims -> Shape
fromDims = foldr (:.) Z

-- | Prefix a shape with inner-most dimensions
-- Example:
-- >>> prefixShape [3,4,5] (2 :. Z)
-- 3x4x5x2
prefixShape :: Dims -> Shape -> Shape
prefixShape ds = go (reverse ds)
  where
    go (x : xs) b = go xs (x :. b)
    go [] b = b

-- | Infix operator for 'prefixShape'
(#:.) :: Dims -> Shape -> Shape
(#:.) = prefixShape

infixr 4 #:.

-- | Unify dimensions with shape returning matched dimensions and dimensions of
-- the optional Any tail.
unifyDims :: Shape -> Dims -> Either String (Dims, Dims)
unifyDims Z [] = pure ([], [])
unifyDims Z _ = Left $ unwords ["Too many dimensions"]
unifyDims (_ :. _) [] = Left $ unwords ["Too few dimensions"]
unifyDims (x :. xs) (y : ys)
  | x == y = fmap (\(as, bs) -> (x : as, bs)) (unifyDims xs ys)
  | otherwise = Left $ unwords ["Dimensions", show x, "and", show y, "do not match"]
unifyDims Any {} ys = pure ([], ys)
