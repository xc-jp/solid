{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tensor.Elt
  ( Elt (..)
  , withRandomElt
  , withShowElt
  , withBinaryElt
  , maybeFloatingElt
  , Some (..)
  ) where

import Data.Binary
import Data.GADT.Compare
import Data.GADT.Show
import Data.Int
import Data.Some
import Data.Type.Equality
import System.Random      (Random)

data Elt a where
  EltFloat :: Elt Float
  EltInt32 :: Elt Int32
  EltInt64 :: Elt Int64
  EltWord32 :: Elt Word32
  EltWord64 :: Elt Word64

deriving instance Eq (Elt a)
deriving instance Show (Elt a)

instance TestEquality Elt where
  testEquality EltFloat  EltFloat  = Just Refl
  testEquality EltInt32  EltInt32  = Just Refl
  testEquality EltInt64  EltInt64  = Just Refl
  testEquality EltWord32 EltWord32 = Just Refl
  testEquality EltWord64 EltWord64 = Just Refl
  testEquality _         _         = Nothing

-- | This gives us Eq for Some Elt
instance GEq Elt where geq = testEquality

instance GShow Elt where gshowsPrec = showsPrec

withRandomElt :: Elt a -> (Random a => r) -> r
withRandomElt EltFloat  = id
withRandomElt EltInt32  = id
withRandomElt EltWord32 = id
withRandomElt EltInt64  = id
withRandomElt EltWord64 = id

maybeFloatingElt :: Elt a -> r -> (Floating a => r) -> r
maybeFloatingElt EltFloat _ r  = r
maybeFloatingElt EltInt32 z _  = z
maybeFloatingElt EltWord32 z _ = z
maybeFloatingElt EltInt64 z _  = z
maybeFloatingElt EltWord64 z _ = z

withShowElt :: Elt e -> (Show e => r) -> r
withShowElt EltFloat  = id
withShowElt EltInt32  = id
withShowElt EltInt64  = id
withShowElt EltWord32 = id
withShowElt EltWord64 = id

withBinaryElt :: Elt e -> (Binary e => r) -> r
withBinaryElt EltFloat  = id
withBinaryElt EltInt32  = id
withBinaryElt EltInt64  = id
withBinaryElt EltWord32 = id
withBinaryElt EltWord64 = id

instance Binary (Some Elt) where
  put (Some EltFloat)  = putWord8 0x0
  put (Some EltInt32)  = putWord8 0x1
  put (Some EltInt64)  = putWord8 0x2
  put (Some EltWord32) = putWord8 0x3
  put (Some EltWord64) = putWord8 0x4

  get = getWord8 >>= \case
   0x0 -> pure (Some EltFloat)
   0x1 -> pure (Some EltInt32)
   0x2 -> pure (Some EltInt64)
   0x3 -> pure (Some EltWord32)
   0x4 -> pure (Some EltWord64)
   _ -> fail "unexpected tag for Elt"
