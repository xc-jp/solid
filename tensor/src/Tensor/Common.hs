{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Tensor.Common
  ( -- * Tensor
    Tensor (..),
    tensorPut,
    tensorGet,
    tensorDataL,
    tensorDimsL,

    -- * Random generators
    genNormal,
    genXavier,
    genXavierFanIn,
    genMSRA,

    -- * Approximation test
    floatApprox,
  )
where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Random (MonadRandom, Random, getRandom, getRandomR)
import Data.AEq
import Data.Positive
import Data.Shape
import GHC.Generics
import Lens.Micro

data Tensor v e = Tensor
  { tensorDims :: Dims,
    tensorData :: v e
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance NFData (v e) => NFData (Tensor v e)

instance Eq (v e) => AEq (Tensor v e)

tensorPut :: Monoid m => (Dims -> m) -> (v e -> m) -> (Tensor v e -> m)
tensorPut putDims putV (Tensor d v) = putDims d <> putV v

tensorGet :: Monad m => m Dims -> m (v e) -> m (Tensor v e)
tensorGet = liftA2 Tensor

tensorDimsL :: Lens' (Tensor v a) Dims
tensorDimsL f (Tensor ds v) = flip Tensor v <$> f ds

-- | Change the base of the tensor.
-- This is unsafe since we don't check if the result has the same number of elements
tensorDataL :: Lens (Tensor v a) (Tensor w b) (v a) (w b)
tensorDataL f (Tensor sh v) = Tensor sh <$> f v

genNormal :: (MonadRandom m, Random e, Floating e) => e -> e -> m e
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $! sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

{-# SPECIALIZE genXavier :: Positive -> Positive -> IO Float #-}
genXavier ::
  (Random e, MonadRandom m, Floating e) =>
  -- | fan-in size
  Positive ->
  -- | fan-out size
  Positive ->
  m e
genXavier fanIn fanOut = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac (fanIn + fanOut)

{-# SPECIALIZE genXavierFanIn :: Positive -> IO Float #-}
genXavierFanIn ::
  (Random e, MonadRandom m, Floating e) =>
  -- | fan-in size
  Positive ->
  m e
genXavierFanIn fanIn = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

{-# SPECIALIZE genMSRA :: Positive -> IO Float #-}
genMSRA ::
  (MonadRandom m, Random e, Floating e) =>
  -- | fan-out size
  Positive ->
  m e
genMSRA fanOut = genNormal 0 (sqrt (2 / realToFrac fanOut))

{-# SPECIALIZE floatApprox :: Float -> Float -> Bool #-}
floatApprox :: (AEq e, Ord e, RealFloat e) => e -> e -> Bool
floatApprox a b = a ~== b || diff < eAbs || rel < eRel || (isNaN a && isNaN b)
  where
    eRel = 1e-2 -- allow 1% relative error
    eAbs = 1e-5 -- allow 10^-5 absolute error
    diff = abs (a - b)
    rel = abs ((a - b) / a)
