{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tensor.Proxy
  ( PTensor,
    Tensor (..),
    fromTensor,
    fromElt,
    fromProxy,
  )
where

import Data.Elt
import Data.Proxy
import Data.Shape
import Tensor.Common

type PTensor = Tensor Proxy

instance Show PTensor where
  showsPrec = tensorShowPrec (const showsPrec)

instance Eq PTensor where
  (==) = tensorEq (const (==)) False

fromTensor ::
  Tensor v ->
  Tensor Proxy
fromTensor (Tensor dims elt _) = Tensor dims elt Proxy

fromElt ::
  Dims ->
  Elt e ->
  PTensor
fromElt dims elt = Tensor dims elt Proxy

fromProxy ::
  KnownElt e =>
  Dims ->
  Proxy e ->
  PTensor
fromProxy dims p = Tensor dims knownElt p
