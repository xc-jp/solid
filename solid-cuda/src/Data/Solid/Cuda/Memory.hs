{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Solid.Cuda.Memory
  ( -- * pointer
    CudaDevPtr (..),
    withPtr,
    withPtr_,

    -- * storable
    allocaCuda,
    withCuda,
    peekCuda,

    -- * vectors (storable with a size)
    allocaCudaVector,
    withCudaVector,
    peekCudaVector,

    -- * SArray
    allocaCudaArray,
    withCudaArray,
    peekCudaArray,

    -- * low level primitives

    -- ** malloc
    cudaMalloc,
    cudaMallocBytes,
    cudaMallocVector,
    cudaMallocArray,

    -- ** memcpyToDev
    cudaMemcpyToDev,
    cudaMemcpyToDevBytes,
    cudaMemcpyToDevVector,
    cudaMemcpyToDevArray,

    -- ** memcpyFromDev
    cudaMemcpyFromDev,
    cudaMemcpyFromDevBytes,

    -- ** free
    cudaFree,
  )
where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Except
import Data.Solid.Array
import Data.Solid.Cuda.Internal
import Data.Solid.Shape (dimsSize)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, sizeOf)
import qualified Language.C.Inline as C

C.context C.baseCtx
C.include "<stddef.h>"
C.include "<cuda.h>"
C.include "<cuda_runtime.h>"

newtype CudaDevPtr a = CudaDevPtr {getCudaPtr :: Ptr a}

withPtr :: (Storable a, MonadCuda m) => (Ptr a -> m b) -> m (a, b)
withPtr f = bracketIO malloc free $ \ptr -> do
  b <- f ptr
  a <- liftIO $ peek ptr
  pure (a, b)

withPtr_ :: (Storable a, MonadCuda m) => (Ptr a -> m ()) -> m a
withPtr_ = fmap fst . withPtr

-- * CUDA memory bracket

-- | Allocate memory of the size of @Storable@ object @a@ on CUDA
allocaCuda :: (Storable a, MonadCuda m) => (CudaDevPtr a -> m b) -> m b
allocaCuda = bracket cudaMalloc cudaFree

allocaCudaVector :: forall a b m. (Storable a, MonadCuda m) => Int -> (CudaDevPtr a -> m b) -> m b
allocaCudaVector n = bracket (cudaMallocVector n) cudaFree

allocaCudaArray :: (Storable a, MonadCuda m) => Dims -> (Array CudaDevPtr a -> m b) -> m b
allocaCudaArray dims = bracket (cudaMallocArray dims) (cudaFree . arrayData)

-- | Upload a @Storable@ value @a@ to CUDA memory and get its address in a bracketed action.
-- For vectors, use @withCudaVector@ to avoid intermediary memory allocation.
withCuda :: (Storable a, MonadCuda m) => a -> (CudaDevPtr a -> m b) -> m b
withCuda a = bracket acquire cudaFree
  where
    acquire = bracketIO (new a) free $ \pa -> do
      cpa <- cudaMalloc
      cudaMemcpyToDev pa cpa
      pure cpa

-- | Upload a @Vector@ of @Storable@ values @as@ to CUDA memory and get its starting address and length in a bracketed action.
withCudaVector :: forall a b m. (Storable a, MonadCuda m) => Vector a -> (CudaDevPtr a -> Int -> m b) -> m b
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

withCudaArray :: (Storable a, MonadCuda m) => SArray a -> (Array CudaDevPtr a -> m b) -> m b
withCudaArray (Array dims as) f = withCudaVector as $ \cpt _ ->
  f (Array dims cpt)

-- | Download a @Storable@ value from CUDA
peekCuda :: (Storable a, MonadCuda m) => CudaDevPtr a -> m a
peekCuda cpa =
  bracketIO malloc free $ \pb -> do
    cudaMemcpyFromDev cpa pb
    liftIO $ peek pb

-- | Download a @Vector@ from CUDA
peekCudaVector :: forall a m. (Storable a, MonadCuda m) => CudaDevPtr a -> Int -> m (Vector a)
peekCudaVector cpas n = do
  as <- liftIO $ MV.new n
  let (fpas, _) = MV.unsafeToForeignPtr0 as
  let pas = unsafeForeignPtrToPtr fpas
  cudaMemcpyFromDevBytes (csizeOfN (undefined :: a) n) cpas pas
  liftIO $ V.freeze as

peekCudaArray :: (Storable a, MonadCuda m) => Array CudaDevPtr a -> m (SArray a)
peekCudaArray (Array dims cpta) = Array dims <$> peekCudaVector cpta (dimsSize dims)

-- * Low-level

cudaMalloc :: forall a m. (Storable a, MonadCuda m) => m (CudaDevPtr a)
cudaMalloc = cudaMallocBytes $ csizeOf (undefined :: a)

cudaMallocBytes :: MonadCuda m => CSize -> m (CudaDevPtr a)
cudaMallocBytes bytes = do
  cpa <- withPtr_ $ \pcpa ->
    callCuda [C.exp| int { cudaMalloc($(void **pcpa), $(size_t bytes)) } |]
  when (cpa == nullPtr) $
    throwErrorCuda AllocationFailed
  pure . CudaDevPtr $ castPtr cpa

cudaMallocVector :: forall a m. (Storable a, MonadCuda m) => Int -> m (CudaDevPtr a)
cudaMallocVector n = cudaMallocBytes $ csizeOfN (undefined :: a) n

cudaMallocArray :: (Storable a, MonadCuda m) => Dims -> m (Array CudaDevPtr a)
cudaMallocArray dims = Array dims <$> cudaMallocVector (dimsSize dims)

cudaMemcpyToDev :: forall a m. (Storable a, MonadCuda m) => Ptr a -> CudaDevPtr a -> m ()
cudaMemcpyToDev = cudaMemcpyToDevBytes $ csizeOf (undefined :: a)

cudaMemcpyToDevBytes :: MonadCuda m => CSize -> Ptr a -> CudaDevPtr a -> m ()
cudaMemcpyToDevBytes bytes srcp (CudaDevPtr dstp) =
  callCuda [C.exp| int { cudaMemcpy($(void *devDst), $(void *hostSrc), $(size_t bytes), cudaMemcpyHostToDevice) } |]
  where
    hostSrc = castPtr srcp
    devDst = castPtr dstp

cudaMemcpyToDevVector :: forall a m. (Storable a, MonadCuda m) => Vector a -> CudaDevPtr a -> m ()
cudaMemcpyToDevVector v =
  cudaMemcpyToDevBytes
    (fromIntegral $ V.length v * sizeOf (undefined :: a))
    (unsafeForeignPtrToPtr $ fst $ V.unsafeToForeignPtr0 v)

cudaMemcpyToDevArray :: (Storable a, MonadCuda m) => Array Vector a -> CudaDevPtr a -> m ()
cudaMemcpyToDevArray t = cudaMemcpyToDevVector (arrayData t)

cudaMemcpyFromDev :: forall a m. (Storable a, MonadCuda m) => CudaDevPtr a -> Ptr a -> m ()
cudaMemcpyFromDev = cudaMemcpyFromDevBytes $ csizeOf (undefined :: a)

cudaMemcpyFromDevBytes :: MonadCuda m => CSize -> CudaDevPtr a -> Ptr a -> m ()
cudaMemcpyFromDevBytes bytes (CudaDevPtr srcp) dstp =
  callCuda [C.exp| int { cudaMemcpy($(void *hostDst), $(void *devSrc), $(size_t bytes), cudaMemcpyDeviceToHost) }|]
  where
    devSrc = castPtr srcp
    hostDst = castPtr dstp

cudaFree :: MonadCuda m => CudaDevPtr a -> m ()
cudaFree (CudaDevPtr pa) = callCuda [C.exp| int { cudaFree($(void *p)) }|]
  where
    p = castPtr pa

-- * Utils

bracketIO :: (MonadIO m, MonadMask m) => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketIO acquire release = bracket (liftIO acquire) (liftIO . release)

csizeOf :: Storable a => a -> CSize
csizeOf = fromIntegral . sizeOf

csizeOfN :: forall a. Storable a => a -> Int -> CSize
csizeOfN a = fromIntegral . (sizeOf a *)
