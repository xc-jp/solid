{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
module Tensor.Elt where

import Data.Int
import Data.Word
import System.Random             (Random)

data Elt a where
  EltFloat :: Elt Float
  EltInt32 :: Elt Int32
  EltInt64 :: Elt Int64
  EltWord32 :: Elt Word32
  EltWord64 :: Elt Word64

deriving instance Eq (Elt a)
deriving instance Show (Elt a)

data SomeElt where
  SomeElt :: Elt a -> SomeElt

instance Show SomeElt where
  show (SomeElt elt) = show elt

instance Eq SomeElt where
  a == b = maybeEqElt (\_ _ -> True) False a b

maybeEqElt :: (forall x. Elt x -> Elt x -> r) -> r -> SomeElt -> SomeElt -> r
maybeEqElt f _ (SomeElt a@EltFloat) (SomeElt b@EltFloat) = f a b
maybeEqElt _ z (SomeElt EltFloat) _ = z
maybeEqElt f _ (SomeElt a@EltInt32) (SomeElt b@EltInt32) = f a b
maybeEqElt _ z (SomeElt EltInt32) _ = z
maybeEqElt f _ (SomeElt a@EltInt64) (SomeElt b@EltInt64) = f a b
maybeEqElt _ z (SomeElt EltInt64) _ = z
maybeEqElt f _ (SomeElt a@EltWord32) (SomeElt b@EltWord32) = f a b
maybeEqElt _ z (SomeElt EltWord32) _ = z
maybeEqElt f _ (SomeElt a@EltWord64) (SomeElt b@EltWord64) = f a b
maybeEqElt _ z (SomeElt EltWord64) _ = z

withRandomElt :: Elt a -> (Random a => r) -> r
withRandomElt EltFloat = id
withRandomElt EltInt32 = id
withRandomElt EltWord32 = id
withRandomElt EltInt64 = id
withRandomElt EltWord64 = id

maybeFloatingElt :: Elt a -> r -> (Floating a => r) -> r
maybeFloatingElt EltFloat _ r = r
maybeFloatingElt EltInt32 z _ = z
maybeFloatingElt EltWord32 z _ = z
maybeFloatingElt EltInt64 z _ = z
maybeFloatingElt EltWord64 z _ = z
