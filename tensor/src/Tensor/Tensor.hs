{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Tensor.Tensor where

import Data.Type.Equality
import Tensor.Elt
import Tensor.Shape       (Dims)

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
