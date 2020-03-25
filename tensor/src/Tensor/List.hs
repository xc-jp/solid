{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor.List
  ( LTensor
  , Tensor (..)
  , fromList
  , fromListFail
  , unfold
  , unfoldM
  , fill
  , fillM
  , normal
  , xavier
  , normalize
  ) where

import           Control.Monad        as Monad
import           Control.Monad.Fail   (MonadFail)
import           Control.Monad.Random (MonadRandom, Random)
import           Data.Elt
import           Data.List            (unfoldr)
import           Data.Positive
import           Data.Shape
import           Tensor.Common

type LTensor = Tensor []

instance Show LTensor where
  showsPrec = tensorShowPrec (const showTruncList)

instance Eq LTensor where
  (==) = tensorEq (const (==)) False

showTruncList :: (Show a) => Int -> [a] -> ShowS
showTruncList m xs
  | null (drop m xs) = shows xs
  | otherwise = \rest -> "[" <> unwords (show <$> take m xs) <> " ... ]" <> rest

fromList :: KnownElt e => Dims -> [e] -> Maybe LTensor
fromList dims e = Tensor dims knownElt <$> takeExact (dimsSize dims) e
  where
    takeExact 0 _      = pure []
    takeExact n (e:es) = (e:) <$> takeExact (n-1) es
    takeExact _ _      = Nothing

fromListFail :: (MonadFail m, KnownElt e) => Dims -> [e] -> m (LTensor)
fromListFail dims = maybe (fail "fromListM: list too short") pure . fromList dims

-- | Construct a tensor from an infinite list
unfold
  :: KnownElt e
  => Dims
  -> (g -> (e, g))
  -> g
  -> LTensor
unfold = tensorUnfold unfoldr

-- | Construct a tensor from an infinite list
unfoldM
  :: KnownElt e
  => Dims
  -> (g -> (e, g))
  -> g
  -> LTensor
unfoldM = tensorUnfold unfoldr

fill
  :: KnownElt e
  => Dims
  -> e
  -> LTensor
fill dims e = Tensor dims knownElt (replicate (dimsSize dims) e)

fillM
  :: (Monad m, KnownElt e)
  => Dims
  -> m e
  -> m LTensor
fillM dims gen = Tensor dims knownElt <$>
  Monad.replicateM (dimsSize dims) gen

normal
  :: (MonadRandom m, Random e, Floating e, KnownElt e)
  => e
  -> e
  -> Dims
  -> m (Tensor [])
normal mean std dims = fillM dims (genNormal mean std)

xavier
  :: (MonadRandom m, Random e, Floating e)
  => Positive
  -> Positive
  -> Dims
  -> Elt e
  -> m (Tensor [])
xavier fanIn fanOut dims elt = Tensor dims elt <$>
  replicateM (dimsSize dims) (genXavier fanIn fanOut)

normalize :: LTensor -> Maybe LTensor
normalize (Tensor dims e xs) = withOrdElt e $ maybeFloatingElt e Nothing $ Just $
  let min' = minimum xs
      max' = maximum xs
      epsilon = 1e-11
   in Tensor dims e (fmap (\x -> (x - min') / (max' - min' + epsilon)) xs)
