{-# LANGUAGE ConstraintKinds #-}
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
    Int32,
    Numerical,
    dynamic,
    dynamic_,
    hdynamic,
    bimapDynamic,
    bimapDynamic_,
    hmapDynamic,
    fhdynamic,
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
    dynToElt,
    eltToDyn,
    fromElt,
    pattern EltFloat,
    pattern EltInt,
    Proxy (..),

    -- * Dims
    Positive,
    Dims,
  )
where

import Control.DeepSeq (NFData)
import Data.Approx
import Data.Functor.Identity
import Data.Int (Int32)
import Data.Positive
import Data.Proxy
import Data.Shape as Sh
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import GHC.Generics
import Tensor.Common as TC
import Tensor.List as TL
import Tensor.Vector as TV

data Dynamic f = DFloat !(f Float) | DInt !(f Int32)
  deriving (Generic)

deriving instance (Eq (f Float), Eq (f Int32)) => Eq (Dynamic f)

deriving instance (Show (f Float), Show (f Int32)) => Show (Dynamic f)

instance (EqWith (f Float), EqWith (f Int32)) => EqWith (Dynamic f)

instance (NFData (f Float), NFData (f Int32)) => NFData (Dynamic f)

dtensorDims :: Dynamic (Tensor f) -> Dims
dtensorDims = hdynamic tensorDims

dtensorElt :: Dynamic (Tensor f) -> Elt
dtensorElt = dynToElt

-- | Destructor for 'Dynamic'
{-# INLINE dynamic #-}
dynamic :: (f Float -> r) -> (f Int32 -> r) -> (Dynamic f -> r)
dynamic f _ (DFloat v) = f v
dynamic _ f (DInt v) = f v

{-# INLINE dynamic_ #-}
dynamic_ :: r -> r -> (Dynamic f -> r)
dynamic_ r _ (DFloat _) = r
dynamic_ _ r (DInt _) = r

type Numerical a = (Num a, Ord a, Storable a, Unbox a, Show a, Real a)

{-# INLINE hdynamic #-}
hdynamic :: (forall a. Numerical a => f a -> r) -> (Dynamic f -> r)
hdynamic f (DFloat v) = f v
hdynamic f (DInt v) = f v

-- | Just a flipped version of hdynamic because GHC doesn't like flip
{-# INLINE fhdynamic #-}
fhdynamic :: Dynamic f -> (forall a. Numerical a => f a -> r) -> r
fhdynamic (DFloat v) f = f v
fhdynamic (DInt v) f = f v

{-# INLINE hmapDynamic #-}
hmapDynamic :: (forall a. Numerical a => f a -> g a) -> Dynamic f -> Dynamic g
hmapDynamic f (DFloat v) = DFloat $ f v
hmapDynamic f (DInt v) = DInt $ f v

{-# INLINE bimapDynamic #-}
bimapDynamic :: (f Float -> g Float) -> (f Int32 -> g Int32) -> Dynamic f -> Dynamic g
bimapDynamic f _ (DFloat v) = DFloat $ f v
bimapDynamic _ f (DInt v) = DInt $ f v

{-# INLINE bimapDynamic_ #-}
bimapDynamic_ :: f Float -> f Int32 -> Dynamic g -> Dynamic f
bimapDynamic_ r _ (DFloat _) = DFloat r
bimapDynamic_ _ r (DInt _) = DInt r

{-# INLINE htraverseDynamic #-}
htraverseDynamic ::
  Functor m =>
  (forall a. Numerical a => f a -> m (g a)) ->
  Dynamic f ->
  m (Dynamic g)
htraverseDynamic f (DFloat v) = DFloat <$> f v
htraverseDynamic f (DInt v) = DInt <$> f v

type DSTensor = Dynamic STensor

type DLTensor = Dynamic LTensor

type DUTensor = Dynamic UTensor

type DVTensor = Dynamic VTensor

data Elt = EltFloat | EltInt
  deriving (Eq, Show, Generic)

instance EqWith Elt

instance NFData Elt

-- | Destructor for 'Elt'. Not named elt because we use that as a variable name a lot.
{-# INLINE fromElt #-}
fromElt :: r -> r -> Elt -> r
fromElt x _ EltFloat = x
fromElt _ x EltInt = x

{-# INLINE eltToDyn #-}
eltToDyn :: f Float -> f Int32 -> Elt -> Dynamic f
eltToDyn x _ EltFloat = DFloat x
eltToDyn _ x EltInt = DInt x

{-# INLINE dynToElt #-}
dynToElt :: Dynamic f -> Elt
dynToElt = dynamic_ EltFloat EltInt

type Scalar = Dynamic Identity

{-# INLINE scalar #-}
scalar :: (Float -> r) -> (Int32 -> r) -> Scalar -> r
scalar f _ (DFloat (Identity x)) = f x
scalar _ f (DInt (Identity x)) = f x

pattern SFloat :: Float -> Scalar
pattern SFloat f = DFloat (Identity f)

pattern SInt :: Int32 -> Scalar
pattern SInt f = DInt (Identity f)
