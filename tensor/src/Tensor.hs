{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Tensor
  ( -- * Tensor Types
    Tensor (..),
    STensor,
    VTensor,
    UTensor,
    LTensor,
    Unbox,
    Storable,

    -- * Dynamic
    Dynamic (..),
    dynamic,
    dynamic',
    withDynamic,
    dynamic_,
    bimapDynamic,
    bimapDynamic_,
    hmapDynamic,
    htraverseDynamic,
    DSTensor,
    DUTensor,
    DVTensor,
    DLTensor,
    dtensorDims,
    dtensorElt,

    -- * Scalar
    Scalar,
    scalar,
    Identity (..),
    pattern SFloat,
    pattern SInt,

    -- * Elt
    Elt,
    toElt,
    pattern EltFloat,
    pattern EltInt,
    Proxy (..),

    -- * Dims
    Positive,
    Dims,
  )
where

import Data.Functor.Identity
import Data.Positive
import Data.Proxy
import Data.Shape as Sh
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import GHC.Generics
import Tensor.Common as TC
import Tensor.List as TL
import Tensor.Vector as TV

data Dynamic f = DFloat !(f Float) | DInt !(f Int)
  deriving (Generic)

deriving instance (Eq (f Float), Eq (f Int)) => Eq (Dynamic f)

deriving instance (Show (f Float), Show (f Int)) => Show (Dynamic f)

dtensorDims :: Dynamic (Tensor f) -> Dims
dtensorDims = dynamic' tensorDims

dtensorElt :: Dynamic (Tensor f) -> Elt
dtensorElt = toElt

dynamic :: (f Float -> r) -> (f Int -> r) -> (Dynamic f -> r)
dynamic f _ (DFloat v) = f v
dynamic _ f (DInt v) = f v

dynamic_ :: r -> r -> (Dynamic f -> r)
dynamic_ r _ (DFloat _) = r
dynamic_ _ r (DInt _) = r

dynamic' :: (forall a. (Num a, Ord a, Storable a, Unbox a) => f a -> r) -> (Dynamic f -> r)
dynamic' f (DFloat v) = f v
dynamic' f (DInt v) = f v

-- | Just a flipped version of dynamic' because GHC doesn't like flip
withDynamic :: Dynamic f -> (forall a. (Num a, Ord a, Storable a, Unbox a) => f a -> r) -> r
withDynamic (DFloat v) f = f v
withDynamic (DInt v) f = f v

hmapDynamic :: (forall a. (Num a, Ord a, Storable a, Unbox a) => f a -> g a) -> Dynamic f -> Dynamic g
hmapDynamic f (DFloat v) = DFloat $ f v
hmapDynamic f (DInt v) = DInt $ f v

bimapDynamic :: (f Float -> g Float) -> (f Int -> g Int) -> Dynamic f -> Dynamic g
bimapDynamic f _ (DFloat v) = DFloat $ f v
bimapDynamic _ f (DInt v) = DInt $ f v

bimapDynamic_ :: f Float -> f Int -> Dynamic g -> Dynamic f
bimapDynamic_ r _ (DFloat _) = DFloat r
bimapDynamic_ _ r (DInt _) = DInt r

htraverseDynamic ::
  Functor m =>
  (forall a. (Num a, Ord a, Storable a, Unbox a) => f a -> m (g a)) ->
  Dynamic f ->
  m (Dynamic g)
htraverseDynamic f (DFloat v) = DFloat <$> f v
htraverseDynamic f (DInt v) = DInt <$> f v

type DSTensor = Dynamic STensor

type DLTensor = Dynamic LTensor

type DUTensor = Dynamic UTensor

type DVTensor = Dynamic VTensor

type Elt = Dynamic Proxy

toElt :: Dynamic f -> Elt
toElt = bimapDynamic_ Proxy Proxy

pattern EltFloat, EltInt :: Elt
pattern EltFloat = DFloat Proxy
pattern EltInt = DInt Proxy

{-# COMPLETE EltFloat, EltInt #-}

type Scalar = Dynamic Identity

scalar :: (Float -> r) -> (Int -> r) -> Scalar -> r
scalar f _ (DFloat (Identity x)) = f x
scalar _ f (DInt (Identity x)) = f x

pattern SFloat :: Float -> Scalar
pattern SFloat f = DFloat (Identity f)

pattern SInt :: Int -> Scalar
pattern SInt f = DInt (Identity f)
