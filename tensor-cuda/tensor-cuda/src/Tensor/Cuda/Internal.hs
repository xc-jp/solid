{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Tensor.Cuda.Internal
  ( CudaT,
    callCuda,
    runCudaT,
    CudaException (..),
  )
where

import Control.Exception hiding (catch, mask, uninterruptibleMask)
import Control.Monad.Catch
import Control.Monad.Except
import Foreign.C.Types (CInt)
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)

newtype CudaT m a = CudaT {unCudaT :: ExceptT CudaException m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runCudaT :: CudaT m a -> m (Either CudaException a)
runCudaT = runExceptT . unCudaT

data CudaException
  = CudaError String Int
  | AllocationFailed
  deriving (Show)

instance Exception CudaException

callCuda :: (MonadIO m, MonadThrow m, HasCallStack) => IO CInt -> CudaT m ()
callCuda code =
  withFrozenCallStack $
    liftIO code >>= \case
      0 -> pure ()
      c -> throwM $ CudaError (prettyCallStack callStack) (fromIntegral c)
