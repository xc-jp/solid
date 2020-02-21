{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Tensor.Elt
  ( Elt (..)
  , withRandomElt
  , withNumElt
  , withShowElt
  , withEqElt
  , withOrdElt
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
import GHC.Generics
import System.Random      (Random)

-- | Supported tensor element types
data Elt a where
  EltFloat :: Elt Float
  EltDouble :: Elt Double
  EltInt8 :: Elt Int8
  EltInt16 :: Elt Int16
  EltInt32 :: Elt Int32
  EltInt64 :: Elt Int64
  EltWord8 :: Elt Word8
  EltWord16 :: Elt Word16
  EltWord32 :: Elt Word32
  EltWord64 :: Elt Word64
deriving instance Eq (Elt a)
deriving instance Show (Elt a)

-- We can't derive Generic for Elt since it is a GADT, so EltTag is used here
-- to get a Binary instance for Some Elt without us having to keep track of the
-- numeric tags ourselves.

data EltTag
  = FloatTag
  | DoubleTag
  | Int8Tag
  | Int16Tag
  | Int32Tag
  | Int64Tag
  | Word8Tag
  | Word16Tag
  | Word32Tag
  | Word64Tag
  deriving (Generic)

instance Binary EltTag

instance Generic (Some Elt) where
  type Rep (Some Elt) = Rec0 EltTag
  from (Some EltFloat)  = K1 FloatTag
  from (Some EltDouble) = K1 DoubleTag
  from (Some EltInt8)   = K1 Int8Tag
  from (Some EltInt16)  = K1 Int16Tag
  from (Some EltInt32)  = K1 Int32Tag
  from (Some EltInt64)  = K1 Int64Tag
  from (Some EltWord8)  = K1 Word8Tag
  from (Some EltWord16) = K1 Word16Tag
  from (Some EltWord32) = K1 Word32Tag
  from (Some EltWord64) = K1 Word64Tag
  to (K1 FloatTag)  = Some EltFloat
  to (K1 DoubleTag) = Some EltDouble
  to (K1 Int8Tag)   = Some EltInt8
  to (K1 Int16Tag)  = Some EltInt16
  to (K1 Int32Tag)  = Some EltInt32
  to (K1 Int64Tag)  = Some EltInt64
  to (K1 Word8Tag)  = Some EltWord8
  to (K1 Word16Tag) = Some EltWord16
  to (K1 Word32Tag) = Some EltWord32
  to (K1 Word64Tag) = Some EltWord64

instance Binary (Some Elt)

-- Avoid catch-all case to enable exhaustiveness checking for this
-- instance.
instance TestEquality Elt where
  testEquality EltFloat  EltFloat  = Just Refl
  testEquality EltFloat  _         = Nothing
  testEquality EltDouble EltDouble = Just Refl
  testEquality EltDouble _         = Nothing
  testEquality EltInt8  EltInt8    = Just Refl
  testEquality EltInt8  _          = Nothing
  testEquality EltInt16  EltInt16  = Just Refl
  testEquality EltInt16  _         = Nothing
  testEquality EltInt32  EltInt32  = Just Refl
  testEquality EltInt32  _         = Nothing
  testEquality EltInt64  EltInt64  = Just Refl
  testEquality EltInt64  _         = Nothing
  testEquality EltWord8 EltWord8   = Just Refl
  testEquality EltWord8 _          = Nothing
  testEquality EltWord16 EltWord16 = Just Refl
  testEquality EltWord16 _         = Nothing
  testEquality EltWord32 EltWord32 = Just Refl
  testEquality EltWord32 _         = Nothing
  testEquality EltWord64 EltWord64 = Just Refl
  testEquality EltWord64 _         = Nothing

-- | This gives us Eq for Some Elt
instance GEq Elt where geq = testEquality

instance GShow Elt where gshowsPrec = showsPrec

withNumElt :: Elt a -> (Num a => r) -> r
withNumElt EltFloat  = id
withNumElt EltDouble = id
withNumElt EltInt8   = id
withNumElt EltWord8  = id
withNumElt EltInt16  = id
withNumElt EltWord16 = id
withNumElt EltInt32  = id
withNumElt EltWord32 = id
withNumElt EltInt64  = id
withNumElt EltWord64 = id

withRandomElt :: Elt a -> (Random a => r) -> r
withRandomElt EltFloat  = id
withRandomElt EltDouble = id
withRandomElt EltInt8   = id
withRandomElt EltWord8  = id
withRandomElt EltInt16  = id
withRandomElt EltWord16 = id
withRandomElt EltInt32  = id
withRandomElt EltWord32 = id
withRandomElt EltInt64  = id
withRandomElt EltWord64 = id

maybeFloatingElt :: Elt a -> r -> (Floating a => r) -> r
maybeFloatingElt EltFloat  _ r = r
maybeFloatingElt EltDouble _ r = r
maybeFloatingElt EltInt8   z _ = z
maybeFloatingElt EltWord8  z _ = z
maybeFloatingElt EltInt16  z _ = z
maybeFloatingElt EltWord16 z _ = z
maybeFloatingElt EltInt32  z _ = z
maybeFloatingElt EltWord32 z _ = z
maybeFloatingElt EltInt64  z _ = z
maybeFloatingElt EltWord64 z _ = z

withShowElt :: Elt e -> (Show e => r) -> r
withShowElt EltFloat  = id
withShowElt EltDouble = id
withShowElt EltInt8   = id
withShowElt EltWord8  = id
withShowElt EltInt16  = id
withShowElt EltWord16 = id
withShowElt EltInt32  = id
withShowElt EltWord32 = id
withShowElt EltInt64  = id
withShowElt EltWord64 = id

withEqElt :: Elt a -> (Eq a => r) -> r
withEqElt EltFloat  = id
withEqElt EltDouble = id
withEqElt EltInt8   = id
withEqElt EltWord8  = id
withEqElt EltInt16  = id
withEqElt EltWord16 = id
withEqElt EltInt32  = id
withEqElt EltWord32 = id
withEqElt EltInt64  = id
withEqElt EltWord64 = id

withOrdElt :: Elt a -> (Ord a => r) -> r
withOrdElt EltFloat  = id
withOrdElt EltDouble = id
withOrdElt EltInt8   = id
withOrdElt EltWord8  = id
withOrdElt EltInt16  = id
withOrdElt EltWord16 = id
withOrdElt EltInt32  = id
withOrdElt EltWord32 = id
withOrdElt EltInt64  = id
withOrdElt EltWord64 = id

withBinaryElt :: Elt e -> (Binary e => r) -> r
withBinaryElt EltFloat  = id
withBinaryElt EltDouble = id
withBinaryElt EltInt8   = id
withBinaryElt EltWord8  = id
withBinaryElt EltInt16  = id
withBinaryElt EltWord16 = id
withBinaryElt EltInt32  = id
withBinaryElt EltWord32 = id
withBinaryElt EltInt64  = id
withBinaryElt EltWord64 = id