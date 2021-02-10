{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor.List where

import Control.Monad
import Control.Monad.Fail (MonadFail)
import Control.Monad.Random (MonadRandom, Random)
import Data.Positive
import Data.Shape
import Tensor.Common

type LTensor = Tensor []

fromList :: Dims -> [e] -> Maybe (LTensor e)
fromList dims e = Tensor dims <$> takeExact (dimsSize dims) e
  where
    takeExact 0 _ = pure []
    takeExact n (e : es) = (e :) <$> takeExact (n -1) es
    takeExact _ _ = Nothing

fromListFail :: (MonadFail m) => Dims -> [e] -> m (LTensor e)
fromListFail dims = maybe (fail "fromListM: list too short") pure . fromList dims

fill ::
  Dims ->
  e ->
  LTensor e
fill dims e = Tensor dims (replicate (dimsSize dims) e)

fillM ::
  (Monad m) =>
  Dims ->
  m e ->
  m (LTensor e)
fillM dims gen = Tensor dims <$> replicateM (dimsSize dims) gen

normal ::
  (MonadRandom m, Random e, Floating e) =>
  e ->
  e ->
  Dims ->
  m (LTensor e)
normal mean std dims = fillM dims (genNormal mean std)

xavier ::
  (MonadRandom m, Random e, Floating e) =>
  Positive ->
  Positive ->
  Dims ->
  m (LTensor e)
xavier fanIn fanOut dims = fillM dims (genXavier fanIn fanOut)

normalize :: (Fractional e, Ord e) => LTensor e -> LTensor e
normalize (Tensor dims xs) =
  let min' = minimum xs
      max' = maximum xs
      epsilon = 1e-11
   in Tensor dims (fmap (\x -> (x - min') / (max' - min' + epsilon)) xs)
