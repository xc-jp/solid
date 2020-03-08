{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}

module Tensor.Common
  ( Tensor(..)
  , tensorDims
  , tensorElt
  , tensorEq
  , tensorShowPrec
  , tensorUnfold
  , tensorUnfoldM
  , tensorTrans
  , tensorTransM
  , genNormal
  , genXavier
  , genXavierFanIn
  , genMSRA
  )
where

import           Control.Monad.Random (MonadRandom, Random, getRandom,
                                       getRandomR)
import           Data.Type.Equality

import Data.Elt
import Data.Positive
import Data.Shape

data Tensor v where Tensor :: Dims -> Elt e -> v e -> Tensor v

tensorDims :: Tensor v -> Dims
tensorDims (Tensor dims _ _) = dims

tensorElt :: Tensor v -> Some Elt
tensorElt (Tensor _ elt _) = Some elt

tensorEq
  :: (forall e. Eq e => Elt e -> v e -> v e -> r)
  -> r
  -> Tensor v
  -> Tensor v
  -> r
tensorEq comp r0 (Tensor sa ea va) (Tensor sb eb vb)
  | sa == sb
  , Just Refl <- testEquality ea eb = withEqElt ea $ comp ea va vb
  | otherwise = r0

tensorShowPrec
  :: (forall e. Show e => Elt e -> Int -> v e -> ShowS)
  -> Int -> Tensor v -> ShowS
tensorShowPrec f d (Tensor dims elt vs) = withShowElt elt $
  showParen (d > 10) $ shows dims
                     . showChar ' '
                     . showParen True (shows elt)
                     . showChar ' '
                     . f elt 10 vs

tensorTrans
  :: (forall a. Elt a -> v a -> w a)
  -> Tensor v -> Tensor w
tensorTrans f (Tensor sh elt v) = Tensor sh elt (f elt v)

tensorTransM
  :: Functor m
  => (forall a. Elt a -> v a -> m (w a))
  -> Tensor v -> m (Tensor w)
tensorTransM f (Tensor sh elt v) = Tensor sh elt <$> f elt v

tensorUnfold
  :: KnownElt e
  => (forall g. (g -> Maybe (e, g)) -> g -> v e)
  -> Dims
  -> (forall g. (g -> (e, g)) -> g -> Tensor v)
tensorUnfold unfoldr dims fg g0 = Tensor dims knownElt $ unfoldr fg' (dimsSize dims,g0)
  where
    fg' (0,_) = Nothing
    fg' (n,g) = Just . fmap (pred n,) $ fg g

tensorUnfoldM
  :: (Monad m, KnownElt e)
  => (forall g. (g -> m (Maybe (e, g))) -> g -> m (v e))
  -> Dims
  -> (forall g. (g -> m (e, g)) -> g -> m (Tensor v))
tensorUnfoldM unfoldr dims fg g0 = Tensor dims knownElt <$> unfoldr fg' (dimsSize dims,g0)
  where
    fg' (0,_) = pure Nothing
    fg' (n,g) = Just . fmap (pred n,) <$> fg g

genNormal
  :: (MonadRandom m, Random e, Floating e)
  => e -> e -> m e
genNormal mean std = do
  u1 <- getRandom
  u2 <- getRandom
  pure $ sqrt (-2 * log u1) * cos (2 * pi * u2) * std + mean

genXavier :: (Random e, MonadRandom m, Floating e)
  => Positive -- ^ fan-in size
  -> Positive -- ^ fan-out size
  -> m e
genXavier fanIn fanOut = getRandomR (-scale, scale)
  where
  scale = sqrt 3 / realToFrac (fanIn + fanOut)

genXavierFanIn :: (Random e, MonadRandom m, Floating e)
  => Positive -- ^ fan-in size
  -> m e
genXavierFanIn fanIn = getRandomR (-scale, scale)
  where
    scale = sqrt 3 / realToFrac fanIn

genMSRA
  :: (MonadRandom m, Random e, Floating e)
  => Positive -- ^ fan-out size
  -> m e
genMSRA fanOut = genNormal 0 (sqrt (2/ realToFrac fanOut))

-- -- data Zippy a b = forall c. Zippy (Elt c, a -> b -> c)

-- -- zipWithT
-- --   -- :: (forall a b. Elt a -> Elt b -> Maybe (SomeEltZip a b))
-- --   :: (forall a b. Elt a -> Elt b -> Maybe (Zippy a b))
-- --   -> Tensor
-- --   -> Tensor
-- --   -> Maybe Tensor
-- -- zipWithT f (Tensor dims e xs) (Tensor _ e' ys)
-- --   = case f e e' of
-- --     Nothing               -> Nothing
-- --     Just (Zippy (elt, g)) -> Just $ Tensor dims elt (zipWith g xs ys)

-- -- zipWithEq :: (forall e. Elt e -> Maybe (e -> e -> e)) -> Tensor -> Tensor -> Maybe Tensor
-- -- zipWithEq g = zipWithT $ \e e' -> do
-- --   Refl <- testEquality e e'
-- --   f <- g e
-- --   pure (Zippy (e, f))

-- -- addAbs :: Tensor -> Tensor -> Maybe Tensor
-- -- addAbs = zipWithEq (\e -> withNumElt e (Just ap))
-- --   where
-- --   ap a b = abs a + abs b

-- -- add :: Tensor -> Tensor -> Maybe Tensor
-- -- add = zipWithEq (\e -> withNumElt e (Just (+)))
