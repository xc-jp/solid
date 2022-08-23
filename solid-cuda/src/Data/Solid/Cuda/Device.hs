{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Solid.Cuda.Device
  ( -- * pointer
    getDeviceCuda,
    deviceSynchronize,
  )
where

import Data.Solid.Cuda.Internal
import Data.Solid.Cuda.Memory (withPtr_)
import qualified Language.C.Inline as C

C.context C.baseCtx
C.include "<cuda.h>"
C.include "<cuda_runtime.h>"

getDeviceCuda :: MonadCuda m => m Int
getDeviceCuda = fmap fromIntegral $
  withPtr_ $ \ptr ->
    callCuda [C.exp| int { cudaGetDevice($(int *ptr)) } |]

deviceSynchronize :: MonadCuda m => m ()
deviceSynchronize =
  callCuda [C.exp| int { cudaDeviceSynchronize() } |]
