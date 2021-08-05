{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Tensor.Common
  ( -- * Tensor
    Tensor (..),
    tensorPut,
    tensorGet,

    -- * Generic constructors
    vector,

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
import Data.Positive
import Data.Shape
import GHC.Generics

data Tensor v a = Tensor
  { tensorDims :: !Dims,
    tensorData :: !(v a)
  }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance NFData (v a) => NFData (Tensor v a)

tensorPut :: Monoid m => (Dims -> m) -> (v a -> m) -> (Tensor v a -> m)
tensorPut putDims putV (Tensor d v) = putDims d <> putV v

tensorGet :: Monad m => m Dims -> m (v a) -> m (Tensor v a)
tensorGet = liftA2 Tensor

-- | Turn any Foldable into a rank 1 tensor
vector :: Foldable f => f a -> Tensor f a
vector f = Tensor [fromIntegral $ length f] f

{-# INLINE genNormal #-}
genNormal :: (MonadRandom m, Random a, Floating a) => a -> a -> m a
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $! sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

{-# INLINE genXavier #-}
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

{-# INLINE genXavierFanIn #-}
genXavierFanIn ::
  (Random a, MonadRandom m, Floating a) =>
  -- | fan-in size
  Positive ->
  m a
genXavierFanIn fanIn = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

{-# INLINE genMSRA #-}
genMSRA ::
  (MonadRandom m, Random a, Floating a) =>
  -- | fan-out size
  Positive ->
  m a
genMSRA fanOut = genNormal 0 (sqrt (2 / realToFrac fanOut))
