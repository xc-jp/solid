{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Tensor.Tensor
  ( Tensor(..)
  , normal
  , xavier
  , ones
  , zeroes
  , maybeEqTensor
  , normals
  )
where

import Control.Monad.Random
import Data.Positive
import Data.Type.Equality
import Tensor.Elt
import Tensor.Shape         (Dims)

data Tensor where
  Tensor :: Dims -> Elt e -> [e] -> Tensor

instance Eq Tensor where
    a@(Tensor sh _ _) == b@(Tensor sh' _ _)
        = sh == sh' && maybeEqTensor (\_ as bs -> as == bs) False a b

maybeEqTensor :: (forall x. Eq x => Elt x -> [x] -> [x] -> r) -> r -> Tensor -> Tensor -> r
maybeEqTensor f z (Tensor _ e xs) (Tensor _ e' ys)   =
  case testEquality e e' of
    Just Refl -> withEqElt e $ f e xs ys
    Nothing   -> z

instance Show Tensor where
  showsPrec _ (Tensor dims elt ws) = withShowElt elt $
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 16 ws

showTruncList :: (Show a) => Int -> [a] -> ShowS
showTruncList m xs | null (drop m xs) = shows xs
            | otherwise = \rest -> "[" <> unwords (show <$> take m xs) <> " ... ]" <> rest

boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t)
  where
    r = sqrt (-2 * log u1)
    t = 2 * pi * u2

boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []

normals :: (Random a, MonadRandom m, Floating a) => a -> a -> m [a]
normals mean std = fmap f . boxMullers <$> getRandoms
  where
  f x = x * std + mean

fromListM :: Monad m => m [e] -> Dims -> Elt e -> m Tensor
fromListM get dims e = Tensor dims e . take (fromIntegral (product dims)) <$> get

normal :: (Random e, MonadRandom m, Floating e) => e -> e -> Dims -> Elt e -> m Tensor
normal mean std = fromListM (normals mean std)

xavier :: (Random e, MonadRandom m, Floating e)
  => Positive -- ^ fan-in size
  -> Positive -- ^ fan-out size
  -> Dims     -- ^ tensor dimensions
  -> Elt e    -- ^ element type
  -> m Tensor
xavier fanIn fanOut = fromListM (getRandomRs (-scale, scale))
  where
  scale = sqrt 3 / realToFrac (fanIn + fanOut)

zeroes :: Num e => Dims -> Elt e -> Tensor
zeroes dims e = let xs = replicate (fromIntegral $ product dims) 0
  in Tensor dims e xs

ones :: Num e => Dims -> Elt e -> Tensor
ones dims e = let xs = replicate (fromIntegral $ product dims) 1
  in Tensor dims e xs
