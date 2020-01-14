{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tensor.Elt
  ( Elt (..)
  , withRandomElt
  , maybeFloatingElt
  , Some (..)
  ) where

import Data.GADT.Compare
import Data.GADT.Show
import Data.Int
import Data.Some
import Data.Type.Equality
import Data.Word
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
