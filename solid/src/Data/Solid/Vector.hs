{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Solid.Vector where

import Control.Monad
import Control.Monad.Random (MonadRandom, Random)
import Data.Foldable
import Data.Int
import Data.Solid.Common
import qualified Data.Solid.Lens as L
import Data.Solid.Positive
import Data.Solid.Shape
import qualified Data.Vector as VV
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Lens.Micro
import Nonlinear (Vec)
import Prelude hiding (map)

type UArray = Array VU.Vector

type SArray = Array VS.Vector

type VArray = Array VV.Vector

fromList :: VG.Vector v e => Dims -> [e] -> Maybe (Array v e)
fromList dims es = if VG.length v == n then pure (Array dims v) else Nothing
  where
    n = dimsSize dims
    v = VG.fromListN n es

vectorFromList :: VG.Vector v a => [a] -> Array v a
vectorFromList l = Array [fromIntegral $ length l] (VG.fromList l)

fromListFail :: (VG.Vector v e, MonadFail m) => Dims -> [e] -> m (Array v e)
fromListFail dims es = maybe (fail "fromListFail: list too short") pure $ fromList dims es

singleton :: VG.Vector v a => a -> Array v a
singleton = Array [] . VG.singleton

vector :: (VG.Vector v a) => v a -> Array v a
vector v = Array [fromIntegral $ VG.length v] v

fromArrayList :: VG.Vector v a => Array [] a -> Array v a
fromArrayList = over L.arrayData VG.fromList

toArrayList :: VG.Vector v e => Array v e -> Array [] e
toArrayList = over L.arrayData VG.toList

{-# SPECIALIZE convert :: Array VU.Vector Float -> Array VS.Vector Float #-}
{-# SPECIALIZE convert :: Array VS.Vector Float -> Array VU.Vector Float #-}
{-# SPECIALIZE convert :: Array VU.Vector Int -> Array VS.Vector Int #-}
{-# SPECIALIZE convert :: Array VS.Vector Int -> Array VU.Vector Int #-}
convert :: (VG.Vector v a, VG.Vector w a) => Array v a -> Array w a
convert = over L.arrayData VG.convert

{-# INLINE fill #-}
fill :: VG.Vector v e => Dims -> e -> Array v e
fill dims e = Array dims (VG.replicate (dimsSize dims) e)

{-# INLINE fillM #-}
fillM :: (Monad m, VG.Vector v e) => m e -> Dims -> m (Array v e)
fillM gen dims = do
  !vec <- VG.replicateM (dimsSize dims) gen
  pure $! Array dims vec

traverse :: (Monad m, VG.Vector v a, VG.Vector v b) => (a -> m b) -> Array v a -> m (Array v b)
traverse f (Array dims vec) = Array dims <$> VG.mapM f vec

normal ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  e ->
  e ->
  Dims ->
  m (Array v e)
normal mean std = fillM (genNormal mean std)

xavier ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  Positive ->
  Positive ->
  Dims ->
  m (Array v e)
xavier fanIn fanOut = fillM (genXavier fanIn fanOut)

xavier' :: (VG.Vector v e, MonadRandom m, Random e, Floating e) => Positive -> Dims -> m (Array v e)
xavier' fanIn = fillM (genXavierFanIn fanIn)

-- | The opposite of 'foldInner', takes a function on every element and expands it into a new innermost dimension.
-- The Vec constraint forces the function to always yield the same number of arguments.
expandInner ::
  forall f v a b.
  (VG.Vector v a, VG.Vector v b, Foldable f, Vec f) =>
  (a -> f b) ->
  Array v a ->
  Array v b
expandInner f (Array dims vec) = Array (fromIntegral n : dims) (VG.concatMap f' vec)
  where
    n = length (pure () :: f ())
    f' = VG.fromList . toList . f
{-# INLINE expandInner #-}

foldInner ::
  forall v a b.
  (VG.Vector v a, VG.Vector v b) =>
  (v a -> b) ->
  Array v a ->
  Array v b
foldInner f (Array [] vec) = singleton $ f vec
foldInner f (Array (n : dims) vec) = Array dims (VG.generate (dimsSize dims) f')
  where
    n' = fromIntegral n
    f' base = f (VG.slice (base * n') n' vec)
{-# INLINE foldInner #-}

msra ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  Positive ->
  Dims ->
  m (Array v e)
msra fanIn = fillM (genMSRA fanIn)

map :: (VG.Vector v a, VG.Vector v b) => (a -> b) -> Array v a -> Array v b
map = over L.arrayData . VG.map

meanVar ::
  (Real e, VG.Vector v Double, VG.Vector v e) =>
  Array v e ->
  (Double, Double)
meanVar (Array _ v) =
  let mean v = VG.sum v / realToFrac (VG.length v)
      vDouble = VG.map realToFrac v
      u = mean vDouble
      var = mean $ VG.map (\x -> let d = x - u in d * d) vDouble
   in (u, var)

-- | Normalizes an array of floating values to the 0-1 range
normalize :: (Ord e, Fractional e, VG.Vector v e) => Array v e -> Array v e
normalize (Array dims xs) =
  let min' = VG.minimum xs
      max' = VG.maximum xs
      epsilon = 1e-11
   in Array dims (VG.map (\x -> (x - min') / (max' - min' + epsilon)) xs)

zipWithExact ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c) =>
  (a -> b -> c) ->
  Array v a ->
  Array v b ->
  Maybe (Array v c)
zipWithExact f (Array da va) (Array db vb)
  | da == db = pure $ Array da (VG.zipWith f va vb)
zipWithExact _ _ _ = Nothing

{-# SPECIALIZE onehot :: Int -> SArray Int32 -> SArray Float #-}
{-# SPECIALIZE onehot :: Int -> UArray Int32 -> UArray Float #-}
onehot :: (VG.Vector v Float, VG.Vector v Int32) => Int -> Array v Int32 -> Array v Float
onehot classes (Array dims v) =
  Array
    (fromIntegral classes : dims)
    $ VG.generate n $ \ix ->
      let (bt, cl) = divMod ix classes
       in if fromIntegral (v VG.! cl) == bt then 1 else 0
  where
    n = classes * dimsSize dims
