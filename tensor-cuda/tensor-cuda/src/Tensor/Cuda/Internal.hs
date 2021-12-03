{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Tensor.Cuda.Internal
  ( CudaT,
    MonadCuda (liftCuda),
    callCuda,
    runCudaT,
    CudaException (..),
  )
where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Shape
import Foreign.C.Types (CInt)
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)

newtype CudaT m a = CudaT {unCudaT :: ExceptT CudaException m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError CudaException, MonadThrow, MonadCatch, MonadMask)

runCudaT :: CudaT m a -> m (Either CudaException a)
runCudaT = runExceptT . unCudaT

class (MonadIO m, MonadMask m) => MonadCuda m where
  liftCuda :: CudaT IO a -> m a

instance (MonadIO m, MonadMask m) => MonadCuda (CudaT m) where
  liftCuda (CudaT (ExceptT io)) = CudaT (ExceptT (liftIO io))

instance MonadCuda m => MonadCuda (MaybeT m) where
  liftCuda = MaybeT . fmap Just . liftCuda

instance (MonadCuda m) => MonadCuda (ExceptT e m) where
  liftCuda = ExceptT . fmap Right . liftCuda

instance MonadCuda m => MonadCuda (ReaderT r m) where
  liftCuda = ReaderT . const . liftCuda

instance (MonadCuda m, Monoid w) => MonadCuda (WriterT w m) where
  liftCuda = WriterT . fmap (,mempty) . liftCuda

instance MonadCuda m => MonadCuda (StateT s m) where
  liftCuda c = StateT $ \s -> fmap (,s) (liftCuda c)

instance (MonadCuda m, Monoid w) => MonadCuda (RWST r w s m) where
  liftCuda c = RWST $ \_ s -> fmap (,s,mempty) (liftCuda c)

data CudaException
  = CudaError String Int
  | AllocationFailed
  | InvalidShape String Dims

instance Show CudaException where
  show (CudaError stack errno) =
    "CUDA error with code " <> show errno <> ", " <> stack
  show AllocationFailed =
    "CUDA failed to allocate required memory"
  show (InvalidShape reason dims) =
    "Invalid shape " <> reason <> ", but got: " <> show dims

instance Exception CudaException

instance Eq CudaException where
  CudaError _ code == CudaError _ code' = code == code'
  AllocationFailed == AllocationFailed = True
  InvalidShape _ dims == InvalidShape _ dims' = dims == dims'
  _ == _ = False

callCuda :: (HasCallStack, MonadCuda m) => IO CInt -> m ()
callCuda code =
  withFrozenCallStack . liftCuda . CudaT $
    liftIO code >>= \case
      0 -> pure ()
      c -> ExceptT . pure . Left $ CudaError (prettyCallStack callStack) (fromIntegral c)
