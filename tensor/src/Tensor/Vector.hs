{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor.Vector where

import Control.Monad
import Control.Monad.Fail (MonadFail)
import Control.Monad.Random (MonadRandom, Random)
import Data.Foldable
import Data.Functor.Rep
import Data.Int
import Data.Positive
import Data.Shape
import qualified Data.Vector as VV
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Lens.Micro
import Tensor.Common
import qualified Tensor.Lens as L
import Prelude hiding (map)

type UTensor = Tensor VU.Vector

type STensor = Tensor VS.Vector

type VTensor = Tensor VV.Vector

fromList :: VG.Vector v e => Dims -> [e] -> Maybe (Tensor v e)
fromList dims es = if VG.length v == n then pure (Tensor dims v) else Nothing
  where
    n = dimsSize dims
    v = VG.fromListN n es

fromListFail :: (VG.Vector v e, MonadFail m) => Dims -> [e] -> m (Tensor v e)
fromListFail dims es = maybe (fail "fromListFail: list too short") pure $ fromList dims es

singleton :: VG.Vector v a => a -> Tensor v a
singleton = Tensor [] . VG.singleton

fromTensorList :: VG.Vector v a => Tensor [] a -> Tensor v a
fromTensorList = over L.tensorData VG.fromList

toTensorList :: VG.Vector v e => Tensor v e -> Tensor [] e
toTensorList = over L.tensorData VG.toList

{-# SPECIALIZE convert :: Tensor VU.Vector Float -> Tensor VS.Vector Float #-}
{-# SPECIALIZE convert :: Tensor VS.Vector Float -> Tensor VU.Vector Float #-}
{-# SPECIALIZE convert :: Tensor VU.Vector Int -> Tensor VS.Vector Int #-}
{-# SPECIALIZE convert :: Tensor VS.Vector Int -> Tensor VU.Vector Int #-}
convert :: (VG.Vector v a, VG.Vector w a) => Tensor v a -> Tensor w a
convert = over L.tensorData VG.convert

{-# INLINE fill #-}
fill :: VG.Vector v e => Dims -> e -> Tensor v e
fill dims e = Tensor dims (VG.replicate (dimsSize dims) e)

{-# INLINE fillM #-}
fillM :: (Monad m, VG.Vector v e) => m e -> Dims -> m (Tensor v e)
fillM gen dims = do
  !vec <- VG.replicateM (dimsSize dims) gen
  pure $! Tensor dims vec

traverse :: (Monad m, VG.Vector v a, VG.Vector v b) => (a -> m b) -> Tensor v a -> m (Tensor v b)
traverse f (Tensor dims vec) = Tensor dims <$> VG.mapM f vec

normal ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  e ->
  e ->
  Dims ->
  m (Tensor v e)
normal mean std = fillM (genNormal mean std)

xavier ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  Positive ->
  Positive ->
  Dims ->
  m (Tensor v e)
xavier fanIn fanOut = fillM (genXavier fanIn fanOut)

xavier' :: (VG.Vector v e, MonadRandom m, Random e, Floating e) => Positive -> Dims -> m (Tensor v e)
xavier' fanIn = fillM (genXavierFanIn fanIn)

-- | The opposite of 'foldInner', takes a function on every element and expands it into a new innermost dimension.
-- A representable functor is guarantueed to always be the same size, which ensures that every element is expanded into the same size new dimension.
expandInner ::
  forall f v a b.
  (VG.Vector v a, VG.Vector v b, Foldable f, Representable f) =>
  (a -> f b) ->
  Tensor v a ->
  Tensor v b
expandInner f (Tensor dims vec) = Tensor (fromIntegral n : dims) (VG.concatMap f' vec)
  where
    n = length (tabulate (const ()) :: f ())
    f' = VG.fromList . toList . f
{-# INLINE expandInner #-}

foldInner ::
  forall v a b.
  (VG.Vector v a, VG.Vector v b) =>
  (v a -> b) ->
  Tensor v a ->
  Tensor v b
foldInner f (Tensor [] vec) = singleton $ f vec
foldInner f (Tensor (n : dims) vec) = Tensor dims (VG.generate (dimsSize dims) f')
  where
    n' = fromIntegral n
    f' base = f (VG.slice (base * n') n' vec)
{-# INLINE foldInner #-}

msra ::
  (VG.Vector v e, MonadRandom m, Random e, Floating e) =>
  Positive ->
  Dims ->
  m (Tensor v e)
msra fanIn = fillM (genMSRA fanIn)

map :: (VG.Vector v a, VG.Vector v b) => (a -> b) -> Tensor v a -> Tensor v b
map = over L.tensorData . VG.map

meanVar ::
  (Real e, VG.Vector v Double, VG.Vector v e) =>
  Tensor v e ->
  (Double, Double)
meanVar (Tensor _ v) =
  let mean v = VG.sum v / realToFrac (VG.length v)
      vDouble = VG.map realToFrac v
      u = mean vDouble
      var = mean $ VG.map (\x -> let d = x - u in d * d) vDouble
   in (u, var)

-- | Normalizes a tensor of floating values to the 0-1 range
normalize :: (Ord e, Fractional e, VG.Vector v e) => Tensor v e -> Tensor v e
normalize (Tensor dims xs) =
  let min' = VG.minimum xs
      max' = VG.maximum xs
      epsilon = 1e-11
   in Tensor dims (VG.map (\x -> (x - min') / (max' - min' + epsilon)) xs)

zipWithExact ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c) =>
  (a -> b -> c) ->
  Tensor v a ->
  Tensor v b ->
  Maybe (Tensor v c)
zipWithExact f (Tensor da va) (Tensor db vb)
  | da == db = pure $ Tensor da (VG.zipWith f va vb)
zipWithExact _ _ _ = Nothing

{-# SPECIALIZE onehot :: Int -> STensor Int32 -> STensor Float #-}
{-# SPECIALIZE onehot :: Int -> UTensor Int32 -> UTensor Float #-}
onehot :: (VG.Vector v Float, VG.Vector v Int32) => Int -> Tensor v Int32 -> Tensor v Float
onehot classes (Tensor dims v) =
  Tensor
    (fromIntegral classes : dims)
    $ VG.generate n $ \ix ->
      let (bt, cl) = divMod ix classes
       in if fromIntegral (v VG.! cl) == bt then 1 else 0
  where
    n = classes * dimsSize dims
