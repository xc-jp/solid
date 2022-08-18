{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Solid.Approx
  ( AApprox,
    AApproxElt,
    DSAApprox,
    aapprox,
    aapproxElt,
    dapprox,
    dapproxElt,
  )
where

import Data.AEq
import Data.Solid.Array
import qualified Data.Vector.Fusion.Bundle as VB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

-- | An Array whose Eq instance allows for tolerances on both the dimensions and Float elements.
-- Dimensions only need to be equal after filtering out all size 1 dimensions.
-- Floats need to fulfill 1 of
--   - Be equal according to AEq's (~==)
--   - Have a relative difference of at most 1%
--   - Have an absolute difference of at most 0.00001
--   - The _left-hand_ argument is NaN
-- Note that this should of course only be used for testing, since a non-transitive Eq instance is illegal.
-- Therefore, we don't expose constructors for this datatype directly.
newtype AApprox v e = AApprox (Array v e)
  deriving (Show)

-- | Like 'AApprox', but dimensions need to be exactly equal
newtype AApproxElt v e = AApproxElt (Array v e)
  deriving (Show)

type DSAApprox = Dynamic (AApprox VS.Vector)

aapprox :: Array f a -> AApprox f a
aapprox = AApprox

aapproxElt :: Array f a -> AApproxElt f a
aapproxElt = AApproxElt

dapprox :: Dynamic (Array f) -> Dynamic (AApprox f)
dapprox = bimapDynamic AApprox AApprox

dapproxElt :: Dynamic (Array f) -> Dynamic (AApproxElt f)
dapproxElt = bimapDynamic AApproxElt AApproxElt

instance VG.Vector v Float => Eq (AApprox v Float) where
  AApprox (Array da va) == AApprox (Array db vb) = dimsApprox da db && genericEqBy floatApprox va vb

instance VG.Vector v Int32 => Eq (AApprox v Int32) where
  AApprox (Array da va) == AApprox (Array db vb) = dimsApprox da db && genericEqBy (==) va vb

instance VG.Vector v Float => Eq (AApproxElt v Float) where
  AApproxElt (Array da va) == AApproxElt (Array db vb) = da == db && genericEqBy floatApprox va vb

instance VG.Vector v Int32 => Eq (AApproxElt v Int32) where
  AApproxElt (Array da va) == AApproxElt (Array db vb) = da == db && genericEqBy (==) va vb

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
