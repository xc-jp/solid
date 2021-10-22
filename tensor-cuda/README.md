# Tensor CUDA

This package provides a Haskell interface to tensor memory communication with CUDA.

## High-level functions

The intended way of performing memory communication is to use the high level bracket functions, grouped by different value types.

Similar to `Foreign.Marshal`, for each value type, three functions are provided.
- `allocCuda :: Size -> CudaT CudaDevPtr` allocates a chunk of memory of the given size on CUDA.
- `withCuda :: Value -> CudaT CudaDevPtr` uploads a value to CUDA, and returns a device pointer to that value.
- `peekCuda :: CudaDevPtr -> Size -> CudaT Value` takes a device pointer, reads a given size of the CUDA memory, and marshals it into a Haskell value.

It should be noted that the device pointer, wrapped in the type `CudaDevPtr`, should not be used outside of bracketed scope, because the memory it points to is automatically cleaned when exiting the bracket.
Furthermore, although it is possible to get the underlying pointer of type `Ptr` from a `CudaDevPtr`, it should not be mixed with the usual `Ptr`, because a `CudaDevPtr` stores a GPU memory address, while a normal `Ptr` stores a CPU address.
Using functions with wrong addresses will result in runtime errors.

A typical use of the bracketed memory interface is as follows:

```haskell
withCuda a $ \pa ->
  allocCuda size $ \pb -> do
    cudaComputation pa pb
    peekCuda pb
```

Supported values:

| Value Type | `alloc` | `with` | `peek` |
|:-------|:--------|:-------|:-------|
| `Storable a => a` | `allocCuda` | `withCuda` | `peekCuda` |
| `Data.Vector.Storable.Vector a` | `allocCudaVector` | `withCudaVector` | `peekCudaVector` |
| `Tensor.Vector.STensor a` | `allocCudaTensor` | `withCudaTensor` | `peekCudaTensor` |

For tensors, the dimensions will be carried with the pointer and used to calculate size when marshalling.
A tensor pointer will have the type `Tensor CudaDevPtr a`.

## Low-level functions

It is also possible to directly manipulate the GPU memory with pointers and byte length.
These functions should always be used in a bracket style to guarantee that the allocated memory is freed in the case of exceptions.

| Functions | Type |
|:----------|:-----|
| `cudaMalloc` | `(Storable a) => CudaT m (CudaDevPtr a)` |
| `cudaMemcpyToDev` | `(Storable a) => Ptr a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyFromDev` | `(Storable a) => CudaDevPtr a -> Ptr a -> CudaT m ()` |
| `cudaMallocBytes` | `CSize -> CudaT m (CudaDevPtr a)` |
| `cudaMemcpyToDevBytes` | `CSize -> Ptr a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyFromDevBytes` | `CSize -> CudaDevPtr a -> Ptr a -> CudaT m ()` |
| `cudaFree` | `CudaDevPtr a -> CudaT m ()` |

