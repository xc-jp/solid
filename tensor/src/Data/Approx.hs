{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Approx
  ( -- * Core defininitions
    EqWith (..),
    Eqs (..),

    -- * Equality Dictionaries
    eq,
    aeq,
    approx,

    -- * Test utilities
    assertEqualWith,
    assertEqWith,
    assertApprox,

    -- * Auxiliary comparators
    floatApprox,
    dimsApprox,
  )
where

import Control.Exception (throwIO)
import Data.AEq
import Data.Data (Proxy)
import Data.Int
import Data.Shape
import Data.Vec (Vec (..))
import qualified Data.Vector as VV
import qualified Data.Vector.Fusion.Bundle as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import GHC.Generics
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Numeric.Natural
import Test.HUnit (Assertion)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

assertEqualWith :: (HasCallStack, Show a, Show b) => (a -> b -> Bool) -> a -> b -> Assertion
assertEqualWith f a b
  | f a b = pure ()
  | otherwise =
    let loc' = case reverse (getCallStack callStack) of
          (_, loc) : _ -> Just loc
          [] -> Nothing
     in throwIO (HUnitFailure loc' $ ExpectedButGot Nothing (show a) (show b))

assertEqWith :: (HasCallStack, Show a, EqWith a) => Eqs -> a -> a -> Assertion
assertEqWith eqs = assertEqualWith (eqWith eqs)

assertApprox :: (HasCallStack, Show a, EqWith a) => a -> a -> Assertion
assertApprox = assertEqWith approx

eq :: Eqs
eq = Eqs (==) (==) (==)

aeq :: Eqs
aeq = Eqs (~==) (~==) (==)

approx :: Eqs
approx = Eqs floatApprox floatApprox dimsApprox

dimsApprox :: Dims -> Dims -> Bool
dimsApprox as bs = filter (/= 1) as == filter (/= 1) bs

class EqWith a where
  eqWith :: Eqs -> a -> a -> Bool
  default eqWith :: (Generic a, GEqWith (Rep a)) => Eqs -> a -> a -> Bool
  eqWith eqs a b = geqWith eqs (from a) (from b)

class GEqWith f where
  geqWith :: Eqs -> f p -> f p -> Bool

instance (GEqWith l, GEqWith r) => GEqWith (l :*: r) where
  geqWith eqs (l :*: r) (l' :*: r') = geqWith eqs l l' && geqWith eqs r r'

instance (GEqWith l, GEqWith r) => GEqWith (l :+: r) where
  geqWith eqs (L1 l) (L1 l') = geqWith eqs l l'
  geqWith eqs (R1 l) (R1 l') = geqWith eqs l l'
  geqWith _ _ _ = False

instance GEqWith V1 where
  geqWith _ _ _ = True

instance EqWith r => GEqWith (K1 m r) where
  geqWith eqs (K1 a) (K1 b) = eqWith eqs a b

instance (GEqWith x) => GEqWith (M1 m i x) where
  geqWith f (M1 x) (M1 x') = geqWith f x x'

instance GEqWith U1 where
  geqWith _ _ _ = True

data Eqs = Eqs
  { eqFloat :: Float -> Float -> Bool,
    eqDouble :: Double -> Double -> Bool,
    eqDims :: Dims -> Dims -> Bool
  }

instance EqWith Float where eqWith = eqFloat

instance EqWith Double where eqWith = eqDouble

instance EqWith Natural where eqWith _ = (==)

instance EqWith Int where eqWith _ = (==)

instance EqWith Int32 where eqWith _ = (==)

instance EqWith Int64 where eqWith _ = (==)

instance EqWith Int16 where eqWith _ = (==)

instance EqWith Int8 where eqWith _ = (==)

instance EqWith a => EqWith [a]

instance EqWith a => EqWith (Vec n a) where
  eqWith eqs = go
    where
      go :: forall n. Vec n a -> Vec n a -> Bool
      go Nil Nil = True
      go (Cons a as) (Cons b bs) = eqWith eqs a b && go as bs

instance EqWith a => EqWith (Maybe a)

instance (EqWith b, EqWith a) => EqWith (Either a b)

instance EqWith ()

instance (EqWith a, EqWith b) => EqWith (a, b)

instance (EqWith a, EqWith b, EqWith c) => EqWith (a, b, c)

instance EqWith (Proxy a)

{-# INLINE genericEqBy #-}
genericEqBy :: VG.Vector v a => (a -> a -> Bool) -> v a -> v a -> Bool
genericEqBy f va vb = VB.eqBy f (VG.stream va) (VG.stream vb)

instance (VS.Storable a, EqWith a) => EqWith (VS.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

instance (VU.Unbox a, EqWith a) => EqWith (VU.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

instance (EqWith a) => EqWith (VV.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

{-# SPECIALIZE floatApprox :: Float -> Float -> Bool #-}
{-# SPECIALIZE floatApprox :: Double -> Double -> Bool #-}
floatApprox :: (AEq e, Ord e, RealFloat e) => e -> e -> Bool
floatApprox a b = a ~== b || diff < eAbs || rel < eRel || isNaN a
  where
    eRel = 1e-2 -- allow 1% relative error
    eAbs = 1e-5 -- allow 10^-5 absolute error
    diff = abs (a - b)
    rel = abs ((a - b) / a)
