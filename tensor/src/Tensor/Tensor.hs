{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Tensor.Tensor where

import Tensor.Elt
import Tensor.Shape (Dims)

data Tensor where
  Tensor :: Dims -> Elt e -> [e] -> Tensor

instance Eq Tensor where
    a@(Tensor sh _ _) == b@(Tensor sh' _ _)
        = sh == sh' && maybeEqTensor (\_ as bs -> as == bs) False a b

maybeEqTensor :: (forall x. Eq x => Elt x -> [x] -> [x] -> r) -> r -> Tensor -> Tensor -> r
maybeEqTensor f _ (Tensor _ EltFloat xs) (Tensor _ EltFloat ys) = f EltFloat xs ys
maybeEqTensor _ z (Tensor _ EltFloat _) _ = z
maybeEqTensor f _ (Tensor _ EltInt32 xs) (Tensor _ EltInt32 ys) = f EltInt32 xs ys
maybeEqTensor _ z (Tensor _ EltInt32 _) _ = z
maybeEqTensor f _ (Tensor _ EltInt64 xs) (Tensor _ EltInt64 ys) = f EltInt64 xs ys
maybeEqTensor _ z (Tensor _ EltInt64 _) _ = z
maybeEqTensor f _ (Tensor _ EltWord32 xs) (Tensor _ EltWord32 ys) = f EltWord32 xs ys
maybeEqTensor _ z (Tensor _ EltWord32 _) _ = z
maybeEqTensor f _ (Tensor _ EltWord64 xs) (Tensor _ EltWord64 ys) = f EltWord64 xs ys
maybeEqTensor _ z (Tensor _ EltWord64 _) _ = z

instance Show Tensor where
  showsPrec _ (Tensor dims elt@EltFloat ws) =
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 4 ws
  showsPrec _ (Tensor dims elt@EltInt32 ws) =
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 4 ws
  showsPrec _ (Tensor dims elt@EltWord32 ws) =
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 4 ws
  showsPrec _ (Tensor dims elt@EltInt64 ws) =
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 4 ws
  showsPrec _ (Tensor dims elt@EltWord64 ws) =
    showParen True
    $ shows dims . showString " " . shows elt . showString " " . showTruncList 4 ws

showTruncList :: (Show a) => Int -> [a] -> ShowS
showTruncList m xs | null (drop m xs) = shows xs
            | otherwise = \rest -> "[" <> unwords (show <$> take m xs) <> " ... ]" <> rest
