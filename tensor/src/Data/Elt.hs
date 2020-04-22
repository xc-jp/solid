{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module Data.Elt
  ( Elt (..)
  , withRandomElt
  , withNumElt
  , withRealElt
  , withShowElt
  , withEqElt
  , withAEqElt
  , withOrdElt
  , withBinaryElt
  , withStorableElt
  , maybeFloatingElt
  , equalElt
  , Some (..)
  , KnownElt (..), fromSample, withKnownElt
  ) where

import           Data.AEq                  (AEq)
import           Data.Binary
import           Data.Char                 (toLower)
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Int
import           Data.Some
import           Data.Text.Prettyprint.Doc (Pretty (..))
import           Data.Type.Equality
import           Data.Vector.Storable      (Storable)
import           GHC.Generics
import           System.Random             (Random)

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

instance Pretty (Some Elt) where
  pretty (Some e) = pretty $ fmap toLower $ drop 3 $ show e

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

equalElt :: Elt a -> Elt b -> r -> (a ~ b => r) -> r
equalElt ea eb r0 r = maybe r0 (\Refl -> r) (testEquality ea eb)

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
{-# INLINE withNumElt #-}

withRealElt :: Elt a -> (Real a => r) -> r
withRealElt EltFloat  = id
withRealElt EltDouble = id
withRealElt EltInt8   = id
withRealElt EltWord8  = id
withRealElt EltInt16  = id
withRealElt EltWord16 = id
withRealElt EltInt32  = id
withRealElt EltWord32 = id
withRealElt EltInt64  = id
withRealElt EltWord64 = id
{-# INLINE withRealElt #-}

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
{-# INLINE withRandomElt #-}

-- 'RealFloat' implies 'Floating', 'RealFrac', 'Real', 'Fractional', 'Num',
-- 'Ord', 'Eq'
maybeFloatingElt
  :: Elt a
  -> (Integral a => r)
  -> (RealFloat a => r)
  -> r
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
{-# INLINE maybeFloatingElt #-}

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
{-# INLINE withShowElt #-}

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
{-# INLINE withEqElt #-}

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
{-# INLINE withOrdElt #-}

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
{-# INLINE withBinaryElt #-}

withStorableElt :: Elt e -> (Storable e => r) -> r
withStorableElt EltFloat  = id
withStorableElt EltDouble = id
withStorableElt EltInt8   = id
withStorableElt EltWord8  = id
withStorableElt EltInt16  = id
withStorableElt EltWord16 = id
withStorableElt EltInt32  = id
withStorableElt EltWord32 = id
withStorableElt EltInt64  = id
withStorableElt EltWord64 = id
{-# INLINE withStorableElt #-}

class KnownElt e where knownElt :: Elt e

fromSample :: KnownElt e => e -> Elt e
fromSample _ = knownElt

instance KnownElt Float  where knownElt = EltFloat
instance KnownElt Double where knownElt = EltDouble
instance KnownElt Int8   where knownElt = EltInt8
instance KnownElt Word8  where knownElt = EltWord8
instance KnownElt Int16  where knownElt = EltInt16
instance KnownElt Word16 where knownElt = EltWord16
instance KnownElt Int32  where knownElt = EltInt32
instance KnownElt Word32 where knownElt = EltWord32
instance KnownElt Int64  where knownElt = EltInt64
instance KnownElt Word64 where knownElt = EltWord64

withKnownElt :: Elt e -> (KnownElt e => r) -> r
withKnownElt EltFloat  = id
withKnownElt EltDouble = id
withKnownElt EltInt8   = id
withKnownElt EltWord8  = id
withKnownElt EltInt16  = id
withKnownElt EltWord16 = id
withKnownElt EltInt32  = id
withKnownElt EltWord32 = id
withKnownElt EltInt64  = id
withKnownElt EltWord64 = id
{-# INLINE withKnownElt #-}

withAEqElt :: Elt e -> (AEq e => r) -> r
withAEqElt EltFloat  = id
withAEqElt EltDouble = id
withAEqElt EltInt8   = id
withAEqElt EltWord8  = id
withAEqElt EltInt16  = id
withAEqElt EltWord16 = id
withAEqElt EltInt32  = id
withAEqElt EltWord32 = id
withAEqElt EltInt64  = id
withAEqElt EltWord64 = id
{-# INLINE withAEqElt #-}
