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

    -- * Dynamic
    Dynamic (..),
    dynamic,
    dynamic',
    Proxy (..),
    Identity (..),
    pattern EltFloat,
    pattern EltInt,
    pattern IFloat,
    pattern IInt,

    -- * Dims
    Positive,
    Dims,
  )
where

import Data.Functor.Identity
import Data.Positive
import Data.Proxy
import Data.Shape as Sh
import GHC.Generics
import Tensor.Common as TC
import Tensor.List as TL
import Tensor.Vector as TV

data Dynamic f = DFloat (f Float) | DInt (f Int)
  deriving (Generic)

deriving instance (Eq (f Float), Eq (f Int)) => Eq (Dynamic f)

deriving instance (Show (f Float), Show (f Int)) => Show (Dynamic f)

dynamic :: (f Float -> r) -> (f Int -> r) -> (Dynamic f -> r)
dynamic f _ (DFloat v) = f v
dynamic _ f (DInt v) = f v

dynamic' :: (forall a. f a -> r) -> (Dynamic f -> r)
dynamic' f (DFloat v) = f v
dynamic' f (DInt v) = f v

pattern EltFloat, EltInt :: Dynamic Proxy
pattern EltFloat = DFloat Proxy
pattern EltInt = DInt Proxy

pattern IFloat :: Float -> Dynamic Identity
pattern IFloat f = DFloat (Identity f)

pattern IInt :: Int -> Dynamic Identity
pattern IInt f = DInt (Identity f)
