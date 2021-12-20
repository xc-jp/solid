{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tensor.Cuda.Device
  ( -- * pointer
    getDeviceCuda,
    deviceSynchronize,
  )
where

import qualified Language.C.Inline as C
import Tensor.Cuda.Internal
import Tensor.Cuda.Memory (withPtr_)

C.context C.baseCtx
C.include "<tensor-cuda.h>"

getDeviceCuda :: MonadCuda m => m Int
getDeviceCuda = fmap fromIntegral $
  withPtr_ $ \ptr ->
    callCuda [C.exp| int { getDevice($(int *ptr)) } |]

deviceSynchronize :: MonadCuda m => m ()
deviceSynchronize =
  callCuda [C.exp| int { deviceSynchronize() } |]
