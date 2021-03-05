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
  )
where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Random (MonadRandom, Random, getRandom, getRandomR)
import Data.Approx
import Data.Positive
import Data.Shape
import GHC.Generics
import Lens.Micro

data Tensor v a = Tensor
  { tensorDims :: Dims,
    tensorData :: v a
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance NFData (v a) => NFData (Tensor v a)

instance EqWith (v a) => EqWith (Tensor v a) where
  eqWith eqs (Tensor da va) (Tensor db vb) = da == db && eqWith eqs va vb

tensorPut :: Monoid m => (Dims -> m) -> (v a -> m) -> (Tensor v a -> m)
tensorPut putDims putV (Tensor d v) = putDims d <> putV v

tensorGet :: Monad m => m Dims -> m (v a) -> m (Tensor v a)
tensorGet = liftA2 Tensor

tensorDimsL :: Lens' (Tensor v a) Dims
tensorDimsL f (Tensor ds v) = flip Tensor v <$> f ds

-- | Change the base of the tensor.
-- This is unsafe since we don't check if the result has the same number of elements
tensorDataL :: Lens (Tensor v a) (Tensor w b) (v a) (w b)
tensorDataL f (Tensor sh v) = Tensor sh <$> f v

genNormal :: (MonadRandom m, Random a, Floating a) => a -> a -> m a
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $! sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

{-# SPECIALIZE genXavier :: Positive -> Positive -> IO Float #-}
genXavier ::
  (Random a, MonadRandom m, Floating a) =>
  -- | fan-in size
  Positive ->
  -- | fan-out size
  Positive ->
  m a
genXavier fanIn fanOut = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac (fanIn + fanOut)

{-# SPECIALIZE genXavierFanIn :: Positive -> IO Float #-}
genXavierFanIn ::
  (Random a, MonadRandom m, Floating a) =>
  -- | fan-in size
  Positive ->
  m a
genXavierFanIn fanIn = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

{-# SPECIALIZE genMSRA :: Positive -> IO Float #-}
genMSRA ::
  (MonadRandom m, Random a, Floating a) =>
  -- | fan-out size
  Positive ->
  m a
genMSRA fanOut = genNormal 0 (sqrt (2 / realToFrac fanOut))
