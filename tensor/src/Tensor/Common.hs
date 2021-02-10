{-# LANGUAGE DeriveGeneric #-}

module Tensor.Common
  ( Tensor (..),
    tensorShowPrec,
    tensorTrans,
    tensorTransM,
    tensorPut,
    tensorGet,
    genNormal,
    genXavier,
    genXavierFanIn,
    genMSRA,
  )
where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad.Random
  ( MonadRandom,
    Random,
    getRandom,
    getRandomR,
  )
import Data.AEq
import Data.Positive
import Data.Shape
import GHC.Generics

data Tensor v e = Tensor
  { tensorDims :: Dims,
    tensorData :: v e
  }
  deriving (Eq, Show, Generic)

instance NFData (v e) => NFData (Tensor v e)

instance Eq (v e) => AEq (Tensor v e)

tensorPut ::
  Monoid m =>
  (Dims -> m) ->
  (v e -> m) ->
  (Tensor v e -> m)
tensorPut putDims putV (Tensor d v) = putDims d <> putV v

tensorGet ::
  Monad m =>
  m Dims ->
  m (v e) ->
  m (Tensor v e)
tensorGet = liftA2 Tensor

tensorShowPrec ::
  (Int -> v e -> ShowS) ->
  Int ->
  Tensor v e ->
  ShowS
tensorShowPrec f d (Tensor dims vs) =
  showParen (d > 10) $
    shows dims
      . showChar ' '
      . showChar ' '
      . f 10 vs

-- TODO Lenses, traversals

tensorTrans ::
  (v e -> w e) ->
  Tensor v e ->
  Tensor w e
tensorTrans f (Tensor sh v) = Tensor sh (f v)

-- FIXME actually this is the lens
tensorTransM :: Functor f => (v a -> f (w a)) -> (Tensor v a -> f (Tensor w a))
tensorTransM f (Tensor sh v) = Tensor sh <$> f v

genNormal ::
  (MonadRandom m, Random e, Floating e) =>
  e ->
  e ->
  m e
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $! sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

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

genXavierFanIn ::
  (Random e, MonadRandom m, Floating e) =>
  -- | fan-in size
  Positive ->
  m e
genXavierFanIn fanIn = getRandomR (- scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

genMSRA ::
  (MonadRandom m, Random e, Floating e) =>
  -- | fan-out size
  Positive ->
  m e
genMSRA fanOut = genNormal 0 (sqrt (2 / realToFrac fanOut))
