{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Approx
  ( -- * Core defininitions
    EqWith (..),
    Eqs (..),

    -- * Equality Dictionaries
    eq,
    aeq,
    approx,

    -- * Test utilities
    assertApprox,

    -- * Auxiliary comparators
    floatApprox,
    dimsApprox,
  )
where

import Control.Exception (throwIO)
import Data.AEq
import Data.Int
import Data.Shape
import Data.Vec (Vec (..))
import qualified Data.Vector as VV
import qualified Data.Vector.Fusion.Bundle as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import GHC.Stack (HasCallStack, callStack, getCallStack)
import Numeric.Natural
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))

assertApprox :: (HasCallStack, Show a, EqWith a) => a -> a -> IO ()
assertApprox a b
  | eqWith approx a b = pure ()
  | otherwise =
    let loc' = case reverse (getCallStack callStack) of
          (_, loc) : _ -> Just loc
          [] -> Nothing
     in throwIO (HUnitFailure loc' $ ExpectedButGot (Just "Approximation") (show a) (show b))

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

data Eqs = Eqs
  { eqFloat :: Float -> Float -> Bool,
    eqDouble :: Double -> Double -> Bool,
    eqDims :: Dims -> Dims -> Bool
  }

instance EqWith Float where eqWith eqs a b = eqFloat eqs a b

instance EqWith Double where eqWith eqs a b = eqDouble eqs a b

instance EqWith Natural where eqWith _ = (==)

instance EqWith Int where eqWith _ = (==)

instance EqWith Int32 where eqWith _ = (==)

instance EqWith Int64 where eqWith _ = (==)

instance EqWith Int16 where eqWith _ = (==)

instance EqWith Int8 where eqWith _ = (==)

instance EqWith a => EqWith [a] where
  eqWith eqs = go
    where
      go [] [] = True
      go (a : as) (b : bs) = eqWith eqs a b && go as bs
      go _ _ = False

instance EqWith a => EqWith (Vec n a) where
  eqWith eqs = go
    where
      go :: forall n. Vec n a -> Vec n a -> Bool
      go Nil Nil = True
      go (Cons a as) (Cons b bs) = eqWith eqs a b && go as bs

instance EqWith a => EqWith (Maybe a) where
  eqWith _ Nothing Nothing = True
  eqWith eqs (Just a) (Just b) = eqWith eqs a b
  eqWith _ _ _ = False

{-# INLINE genericEqBy #-}
genericEqBy :: VG.Vector v a => (a -> a -> Bool) -> v a -> v a -> Bool
genericEqBy f va vb = VB.eqBy f (VG.stream va) (VG.stream vb)

instance (VS.Storable a, EqWith a) => EqWith (VS.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

instance (VU.Unbox a, EqWith a) => EqWith (VU.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

instance (EqWith a) => EqWith (VV.Vector a) where eqWith eqs = genericEqBy (eqWith eqs)

{-# SPECIALIZE floatApprox :: Float -> Float -> Bool #-}
{-# SPECIALIZE floatApprox :: Double -> Double -> Bool #-}
floatApprox :: (AEq e, Ord e, RealFloat e) => e -> e -> Bool
floatApprox a b = a ~== b || diff < eAbs || rel < eRel || (isNaN a && isNaN b)
  where
    eRel = 1e-2 -- allow 1% relative error
    eAbs = 1e-5 -- allow 10^-5 absolute error
    diff = abs (a - b)
    rel = abs ((a - b) / a)
