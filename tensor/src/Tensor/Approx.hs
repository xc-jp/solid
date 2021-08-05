{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tensor.Approx
  ( TApprox (..),
    TApproxElt (..),
    DSTApprox,
    dapprox,
    dapproxElt,
  )
where

import Data.AEq
import qualified Data.Vector.Fusion.Bundle as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Tensor

-- | A Tensor whose Eq instance allows for tolerances on both the dimensions and Float elements.
-- Dimensions only need to be equal after filtering out all size 1 dimensions.
-- Floats need to fulfill 1 of
--   - Be equal according to AEq's (~==)
--   - Have a relative difference of at most 1%
--   - Have an absolute difference of at most 0.00001
--   - The _left-hand_ argument is NaN
newtype TApprox v e = TApprox {unTApprox :: Tensor v e}
  deriving newtype (Show)

-- | Like 'TApprox', but dimensions need to be exactly equal
newtype TApproxElt v e = TApproxElt {unTApproxElt :: Tensor v e}

type DSTApprox = Dynamic (TApprox VS.Vector)

dapprox :: Dynamic (Tensor f) -> Dynamic (TApprox f)
dapprox = bimapDynamic TApprox TApprox

dapproxElt :: Dynamic (Tensor f) -> Dynamic (TApproxElt f)
dapproxElt = bimapDynamic TApproxElt TApproxElt

instance VG.Vector v Float => Eq (TApprox v Float) where
  TApprox (Tensor da va) == TApprox (Tensor db vb) = dimsApprox da db && genericEqBy floatApprox va vb

instance VG.Vector v Int32 => Eq (TApprox v Int32) where
  TApprox (Tensor da va) == TApprox (Tensor db vb) = dimsApprox da db && genericEqBy (==) va vb

instance VG.Vector v Float => Eq (TApproxElt v Float) where
  TApproxElt (Tensor da va) == TApproxElt (Tensor db vb) = da == db && genericEqBy floatApprox va vb

instance VG.Vector v Int32 => Eq (TApproxElt v Int32) where
  TApproxElt (Tensor da va) == TApproxElt (Tensor db vb) = da == db && genericEqBy (==) va vb

dimsApprox :: Dims -> Dims -> Bool
dimsApprox as bs = filter (/= 1) as == filter (/= 1) bs

{-# INLINE genericEqBy #-}
genericEqBy :: VG.Vector v a => (a -> a -> Bool) -> v a -> v a -> Bool
genericEqBy f va vb = VB.eqBy f (VG.stream va) (VG.stream vb)

{-# SPECIALIZE floatApprox :: Float -> Float -> Bool #-}
{-# SPECIALIZE floatApprox :: Double -> Double -> Bool #-}
floatApprox :: (AEq e, Ord e, RealFloat e) => e -> e -> Bool
floatApprox a b = a ~== b || diff < eAbs || rel < eRel || isNaN a
  where
    eRel = 1e-2 -- allow 1% relative error
    eAbs = 1e-5 -- allow 10^-5 absolute error
    diff = abs (a - b)
    rel = abs ((a - b) / a)
