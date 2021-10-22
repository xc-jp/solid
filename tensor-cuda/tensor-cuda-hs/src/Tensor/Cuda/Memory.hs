{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tensor.Cuda.Memory
  ( -- * pointer
    CudaDevPtr (..),

    -- * storable
    allocCuda,
    withCuda,
    peekCuda,

    -- * vectors (storable with a size)
    allocCudaVector,
    withCudaVector,
    peekCudaVector,

    -- * STensor
    allocCudaTensor,
    withCudaTensor,
    peekCudaTensor,

    -- * low level primitives
    cudaMalloc,
    cudaMallocBytes,
    cudaMemcpyToDev,
    cudaMemcpyToDevBytes,
    cudaMemcpyFromDev,
    cudaMemcpyFromDevBytes,
    cudaFree,
  )
where

import Control.Monad (when)
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, throwM)
import Control.Monad.IO.Class
import Data.Shape (Dims, dimsSize)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import qualified Language.C.Inline as C
import Tensor.Common (Tensor (..))
import Tensor.Cuda.Internal
  ( CudaException (..),
    CudaT,
    callCuda,
  )
import Tensor.Vector (STensor)

C.context C.baseCtx
C.include "<tensor-cuda/memory.h>"
C.include "<stddef.h>"

newtype CudaDevPtr a = CudaDevPtr {getCudaPtr :: Ptr a}

-- * CUDA memory bracket

-- | Allocate memory of the size of @Storable@ object @a@ on CUDA
allocCuda :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => (CudaDevPtr a -> CudaT m b) -> CudaT m b
allocCuda = bracket cudaMalloc cudaFree

allocCudaVector :: forall a b m. (Storable a, MonadIO m, MonadMask m, MonadThrow m) => Int -> (CudaDevPtr a -> CudaT m b) -> CudaT m b
allocCudaVector n = bracket (cudaMallocBytes $ csizeOfN (undefined :: a) n) cudaFree

allocCudaTensor :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => Dims -> (Tensor CudaDevPtr a -> CudaT m b) -> CudaT m b
allocCudaTensor dims f = allocCudaVector (dimsSize dims) $ \cpt ->
  f (Tensor dims cpt)

-- | Upload a @Storable@ value @a@ to CUDA memory and get its address in a bracketed action.
-- For vectors, use @withCudaVector@ to avoid intermediary memory allocation.
withCuda :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => a -> (CudaDevPtr a -> CudaT m b) -> CudaT m b
withCuda a = bracket acquire cudaFree
  where
    acquire = bracketIO (new a) free $ \pa -> do
      cpa <- cudaMalloc
      cudaMemcpyToDev pa cpa
      pure cpa

-- | Upload a @Vector@ of @Storable@ values @as@ to CUDA memory and get its starting address and length in a bracketed action.
withCudaVector :: forall a b m. (Storable a, MonadIO m, MonadMask m, MonadThrow m) => Vector a -> (CudaDevPtr a -> Int -> CudaT m b) -> CudaT m b
withCudaVector as f = bracket acquire cudaFree (flip f $ fromIntegral . V.length $ as)
  where
    acquire =
      let (fpas, n) = V.unsafeToForeignPtr0 as
          bytes = csizeOfN (undefined :: a) n
          pas = unsafeForeignPtrToPtr fpas
       in do
            cpas <- cudaMallocBytes bytes
            cudaMemcpyToDevBytes bytes pas cpas
            pure cpas

withCudaTensor :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => STensor a -> (Tensor CudaDevPtr a -> CudaT m b) -> CudaT m b
withCudaTensor (Tensor dims as) f = withCudaVector as $ \cpt _ ->
  f (Tensor dims cpt)

-- | Download a @Storable@ value from CUDA
peekCuda :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => CudaDevPtr a -> CudaT m a
peekCuda cpa =
  bracketIO malloc free $ \pb -> do
    cudaMemcpyFromDev cpa pb
    liftIO $ peek pb

-- | Download a @Vector@ from CUDA
peekCudaVector :: forall a m. (Storable a, MonadIO m, MonadMask m, MonadThrow m) => CudaDevPtr a -> Int -> CudaT m (Vector a)
peekCudaVector cpas n = do
  as <- liftIO $ MV.new n
  let (fpas, _) = MV.unsafeToForeignPtr0 as
  let pas = unsafeForeignPtrToPtr fpas
  cudaMemcpyFromDevBytes (csizeOfN (undefined :: a) n) cpas pas
  liftIO $ V.freeze as

peekCudaTensor :: (Storable a, MonadIO m, MonadMask m, MonadThrow m) => Tensor CudaDevPtr a -> CudaT m (STensor a)
peekCudaTensor (Tensor dims cpta) = Tensor dims <$> peekCudaVector cpta (dimsSize dims)

-- * Low-level

cudaMemcpyToDev :: forall a m. (Storable a, MonadIO m, MonadThrow m) => Ptr a -> CudaDevPtr a -> CudaT m ()
cudaMemcpyToDev = cudaMemcpyToDevBytes $ csizeOf (undefined :: a)

cudaMemcpyToDevBytes :: (MonadIO m, MonadThrow m) => CSize -> Ptr a -> CudaDevPtr a -> CudaT m ()
cudaMemcpyToDevBytes bytes srcp (CudaDevPtr dstp) =
  callCuda [C.exp| int { memcpyToDev($(size_t bytes), $(void *devDst), $(void *hostSrc)) } |]
  where
    hostSrc = castPtr srcp
    devDst = castPtr dstp

cudaMemcpyFromDev :: forall a m. (Storable a, MonadIO m, MonadThrow m) => CudaDevPtr a -> Ptr a -> CudaT m ()
cudaMemcpyFromDev = cudaMemcpyFromDevBytes $ csizeOf (undefined :: a)

cudaMemcpyFromDevBytes :: (MonadIO m, MonadThrow m) => CSize -> CudaDevPtr a -> Ptr a -> CudaT m ()
cudaMemcpyFromDevBytes bytes (CudaDevPtr srcp) dstp =
  callCuda [C.exp| int { memcpyFromDev($(size_t bytes), $(void *hostDst), $(void *devSrc)) }|]
  where
    devSrc = castPtr srcp
    hostDst = castPtr dstp

cudaMalloc :: forall a m. (Storable a, MonadIO m, MonadThrow m) => CudaT m (CudaDevPtr a)
cudaMalloc = cudaMallocBytes $ csizeOf (undefined :: a)

cudaMallocBytes :: (MonadIO m, MonadThrow m) => CSize -> CudaT m (CudaDevPtr a)
cudaMallocBytes bytes = do
  pcpa <- liftIO $ new nullPtr
  callCuda [C.exp| int { devMalloc($(size_t bytes), $(void **pcpa)) } |]
  cpa <- liftIO $ peek pcpa
  when (cpa == nullPtr) $
    throwM AllocationFailed
  pure . CudaDevPtr $ castPtr cpa

cudaFree :: (MonadIO m, MonadThrow m) => CudaDevPtr a -> CudaT m ()
cudaFree (CudaDevPtr pa) = callCuda [C.exp| int { devFree($(void *p)) }|]
  where
    p = castPtr pa

-- * Utils

bracketIO :: (MonadIO m, MonadMask m) => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketIO acquire release = bracket (liftIO acquire) (liftIO . release)

csizeOf :: Storable a => a -> CSize
csizeOf = fromIntegral . sizeOf

csizeOfN :: forall a. Storable a => a -> Int -> CSize
csizeOfN a = fromIntegral . (sizeOf a *)
