{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Tensor.Common
  ( Tensor (..),
    tensorDims,
    tensorElt,
    tensorEq,
    tensorAEq,
    tensorShowPrec,
    tensorUnfold,
    tensorUnfoldM,
    tensorTrans,
    tensorTransM,
    tensorPut,
    tensorGet,
    genNormal,
    genXavier,
    genXavierFanIn,
    genMSRA,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Random
  ( MonadRandom,
    Random,
    getRandom,
    getRandomR,
  )
import Data.AEq
import Data.Elt
import Data.Positive
import Data.Shape
import Data.Some
import Data.Type.Equality
import qualified Data.Vector.Storable as SVec

data Tensor v where Tensor :: Dims -> Elt e -> v e -> Tensor v

instance NFData (Tensor SVec.Vector) where
  rnf (Tensor dims elt vec) = seq dims $! seq elt $! rnf vec

tensorDims :: Tensor v -> Dims
tensorDims (Tensor dims _ _) = dims

tensorElt :: Tensor v -> Some Elt
tensorElt (Tensor _ elt _) = Some elt

tensorPut ::
  Monoid m =>
  (Dims -> m) ->
  (Some Elt -> m) ->
  (forall e. Elt e -> v e -> m) ->
  (Tensor v -> m)
tensorPut putDims putElt putV (Tensor d e v) = putDims d <> putElt (Some e) <> putV e v

tensorGet ::
  Monad m =>
  m Dims ->
  m (Some Elt) ->
  (forall e. Elt e -> m (v e)) ->
  m (Tensor v)
tensorGet getDims getElt getV = do
  dims <- getDims
  selt <- getElt
  withSome selt $ \elt -> Tensor dims elt <$> getV elt

tensorEq ::
  (forall e. Eq e => Elt e -> v e -> v e -> r) ->
  r ->
  Tensor v ->
  Tensor v ->
  r
tensorEq comp r0 (Tensor sa ea va) (Tensor sb eb vb)
  | sa == sb,
    Just Refl <- testEquality ea eb =
    withEqElt ea $ comp ea va vb
  | otherwise = r0

tensorAEq ::
  (forall e. AEq e => Elt e -> v e -> v e -> r) ->
  r ->
  Tensor v ->
  Tensor v ->
  r
tensorAEq comp r0 (Tensor sa ea va) (Tensor sb eb vb)
  | sa == sb,
    Just Refl <- testEquality ea eb =
    withAEqElt ea $ comp ea va vb
  | otherwise = r0

tensorShowPrec ::
  (forall e. Show e => Elt e -> Int -> v e -> ShowS) ->
  Int ->
  Tensor v ->
  ShowS
tensorShowPrec f d (Tensor dims elt vs) =
  withShowElt elt $
    showParen (d > 10) $
      shows dims
        . showChar ' '
        . showParen True (shows elt)
        . showChar ' '
        . f elt 10 vs

tensorTrans ::
  (forall a. Elt a -> v a -> w a) ->
  Tensor v ->
  Tensor w
tensorTrans f (Tensor sh elt v) = Tensor sh elt (f elt v)

tensorTransM ::
  Functor m =>
  (forall a. Elt a -> v a -> m (w a)) ->
  Tensor v ->
  m (Tensor w)
tensorTransM f (Tensor sh elt v) = Tensor sh elt <$> f elt v

tensorUnfold ::
  KnownElt e =>
  (forall g. (g -> Maybe (e, g)) -> g -> v e) ->
  Dims ->
  (forall g. (g -> (e, g)) -> g -> Tensor v)
tensorUnfold unfoldr dims fg g0 = Tensor dims knownElt $ unfoldr fg' (dimsSize dims, g0)
  where
    fg' (0, _) = Nothing
    fg' (n, g) = Just . fmap (pred n,) $ fg g

tensorUnfoldM ::
  (Monad m, KnownElt e) =>
  (forall g. (g -> m (Maybe (e, g))) -> g -> m (v e)) ->
  Dims ->
  (forall g. (g -> m (e, g)) -> g -> m (Tensor v))
tensorUnfoldM unfoldr dims fg g0 = Tensor dims knownElt <$> unfoldr fg' (dimsSize dims, g0)
  where
    fg' (0, _) = pure Nothing
    fg' (n, g) = Just . fmap (pred n,) <$> fg g

genNormal ::
  (MonadRandom m, Random e, Floating e) =>
  e ->
  e ->
  m e
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $! sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

genXavier ::
  (Random e, MonadRandom m, Floating e) =>
  -- | fan-in size
  Positive ->
  -- | fan-out size
  Positive ->
  m e
genXavier fanIn fanOut = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac (fanIn + fanOut)

genXavierFanIn ::
  (Random e, MonadRandom m, Floating e) =>
  -- | fan-in size
  Positive ->
  m e
genXavierFanIn fanIn = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

genMSRA ::
  (MonadRandom m, Random e, Floating e) =>
  -- | fan-out size
  Positive ->
  m e
genMSRA fanOut = genNormal 0 (sqrt (2 / realToFrac fanOut))
