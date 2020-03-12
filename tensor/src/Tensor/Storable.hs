{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor.Storable
  ( STensor
  , Tensor (..)
  , tensorDims
  , tensorElt
  , fromTensorList
  , toTensorList
  , fromList
  , fromListFail
  , singleton
  , Tensor.Storable.replicate
  , fill
  , fillM
  , unfold
  , unfoldM
  , mapMono, mapMonoM, zipMono
  , normal
  , xavier
  , xavier'
  , msra
  , normalize
  , Storable, Vector
  ) where

import           Control.Monad
import           Control.Monad.Random (MonadRandom, Random)
import           Control.Monad.Fail (MonadFail)
import           Data.Vector.Storable (Storable, Vector)
import           Data.Type.Equality
import qualified Data.Vector.Storable as V

import Data.Elt
import Data.Positive
import Data.Shape
import Tensor.Common

type STensor = Tensor Vector

instance Show STensor where
  showsPrec = tensorShowPrec (\e -> withStorableElt e showsPrec)

instance Eq STensor where
  (==) = tensorEq (\e -> withStorableElt e (==)) False

fromList
  :: (KnownElt e, Storable e)
  => Dims
  -> [e]
  -> Maybe STensor
fromList dims es = if V.length v == n then pure (Tensor dims knownElt v) else Nothing
  where
    n = dimsSize dims
    v = V.fromListN n es

fromListFail
  :: (MonadFail m, KnownElt e, Storable e)
  => Dims -> [e] -> m STensor
fromListFail dims es = maybe (fail "fromListFail: list too short") pure $ fromList dims es

singleton :: (KnownElt e, Storable e) => e -> STensor
singleton e = Tensor [] knownElt (V.singleton e)

fromTensorList
  :: Tensor []
  -> Tensor Vector
fromTensorList (Tensor dims elt es) = withStorableElt elt $
  Tensor dims elt (V.fromList es)

toTensorList
  :: Tensor Vector
  -> Tensor []
toTensorList (Tensor dims elt es) = withStorableElt elt $
  Tensor dims elt (V.toList es)

replicate
  :: (KnownElt e, Storable e)
  => Dims
  -> e
  -> STensor
replicate dims act = Tensor dims knownElt $ V.replicate (dimsSize dims) act

unfold
  :: (Storable e, KnownElt e)
  => Dims
  -> (g -> (e, g))
  -> g
  -> STensor
unfold = tensorUnfold V.unfoldr

unfoldM
  :: (Monad m, Storable e, KnownElt e)
  => Dims
  -> (g -> m (e, g))
  -> g
  -> m STensor
unfoldM = tensorUnfoldM V.unfoldrM

fill
  :: (KnownElt e, Storable e)
  => Dims
  -> e
  -> STensor
fill dims e = Tensor dims knownElt (V.replicate (dimsSize dims) e)

fillM
  :: (Monad m, KnownElt e, Storable e)
  => m e
  -> Dims
  -> m STensor
fillM gen dims = Tensor dims knownElt <$>
  V.replicateM (dimsSize dims) gen

normal
  :: (MonadRandom m, Storable e, Random e, Floating e, KnownElt e)
  => e
  -> e
  -> Dims
  -> m STensor
normal mean std dims = fillM (genNormal mean std) dims

xavier
  :: (MonadRandom m, Storable e, Random e, Floating e)
  => Positive
  -> Positive
  -> Dims
  -> Elt e
  -> m STensor
xavier fanIn fanOut dims elt = Tensor dims elt <$>
  V.replicateM (dimsSize dims) (genXavier fanIn fanOut)

xavier'
  :: (MonadRandom m, Storable e, Random e, Floating e)
  => Positive
  -> Dims
  -> Elt e
  -> m STensor
xavier' fanIn dims elt = Tensor dims elt <$>
  V.replicateM (dimsSize dims) (genXavierFanIn fanIn)

msra
  :: (MonadRandom m, Storable e, Random e, Floating e)
  => Positive
  -> Dims
  -> Elt e
  -> m STensor
msra fanIn dims elt = Tensor dims elt <$>
  V.replicateM (dimsSize dims) (genMSRA fanIn)

normalize :: STensor -> Maybe STensor
normalize (Tensor dims e xs) = withStorableElt e $ withOrdElt e $ maybeFloatingElt e Nothing $ Just $
  let min' = V.minimum xs
      max' = V.maximum xs
      epsilon = 1e-11
   in Tensor dims e (V.map (\x -> (x - min') / (max' - min' + epsilon)) xs)

mapMono
  :: (forall a. Elt a -> a -> a)
  -> STensor
  -> STensor
mapMono f (Tensor dims elt xs) = withStorableElt elt $
  Tensor dims elt (V.map (f elt) xs)

mapMonoM
  :: Monad m
  => (forall a. Elt a -> a -> m a )
  -> STensor
  -> m STensor
mapMonoM f (Tensor dims elt xs) = withStorableElt elt $
  Tensor dims elt <$> V.mapM (f elt) xs

zipMono
  :: (forall a. Elt a -> a -> a -> a)
  -> STensor
  -> STensor
  -> Maybe STensor
zipMono f (Tensor d1 e1 x1) (Tensor d2 e2 x2) = do
  guard $ d1 == d2
  Refl <- testEquality e1 e2
  pure $ withStorableElt e1 $ Tensor d1 e1 $ V.zipWith (f e1) x1 x2
