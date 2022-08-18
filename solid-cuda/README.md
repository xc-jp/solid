# `solid-cuda`

This package is a Haskell interface for memory communication with CUDA.

Features:

- A bracket style is adopted so that unused memories are released safely and precisely, even in the case of exceptions.
- A monad transformer `CudaT` is provided for easy composition.

Three types of values are supported: `Storable` values from [`base`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Foreign-Storable.html), `Data.Vector.Storable` from [the `vector` package](https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Storable.html), and `solid` arrays.

When used with `solid` arrays, the dimension is carried along with the pointer.

## High-level functions

It's recommended to use high level bracket functions.

Similar to `Foreign.Marshal`, for each value type, three functions are provided.
- `allocaCuda :: Size -> CudaT CudaDevPtr` allocates a chunk of memory of the given size on CUDA.
- `withCuda :: Value -> CudaT CudaDevPtr` uploads a value to CUDA, and returns a device pointer to that value.
- `peekCuda :: CudaDevPtr -> Size -> CudaT Value` takes a device pointer, reads a given size of the CUDA memory, and marshals it into a Haskell value.

It should be noted that the device pointer, wrapped in the type `CudaDevPtr`, should not be used outside of bracketed scope, because the memory it points to is automatically cleaned when exiting the bracket.
Furthermore, although it is possible to get the underlying pointer of type `Ptr` from a `CudaDevPtr`, it should not be mixed with the usual `Ptr`, because a `CudaDevPtr` stores a GPU memory address, while a normal `Ptr` stores a CPU address.
Using functions with wrong addresses will result in runtime errors.

A typical use of the bracketed memory interface is as follows:

```haskell
withCuda a $ \pa ->
  allocaCuda size $ \pb -> do
    cudaComputation pa pb
    peekCuda pb
```

Supported values:

| Value Type | `alloc` | `with` | `peek` |
|:-----------|:--------|:-------|:-------|
| `Storable a => a` | `allocaCuda` | `withCuda` | `peekCuda` |
| `Data.Vector.Storable.Vector a` | `allocaCudaVector` | `withCudaVector` | `peekCudaVector` |
| `Data.Solid.Vector.SArray a` | `allocaCudaArray` | `withCudaArray` | `peekCudaArray` |

For arrays, the dimensions will be carried with the pointer and used to calculate size when marshalling.
An array pointer will have the type `Array CudaDevPtr a`.

## Low-level functions

It is also possible to directly manipulate the GPU memory with pointers and byte length.
These functions should be used in a bracket style to guarantee that allocated memories are freed in the case of exceptions.

| Functions | Type |
|:----------|:-----|
| `cudaMalloc` | `(Storable a) => CudaT m (CudaDevPtr a)` |
| `cudaMallocBytes` | `CSize -> CudaT m (CudaDevPtr a)` |
| `cudaMallocVector` | `Int -> CudaT m (CudaDevPtr a)` |
| `cudaMallocArray` | `Dims -> CudaT m (Array CudaDevPtr a)` |
| `cudaMemcpyToDev` | `(Storable a) => Ptr a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyToDevBytes` | `CSize -> Ptr a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyToDevVector` | `Vector a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyToDevArray` | `Array Vector a -> CudaDevPtr a -> CudaT m ()` |
| `cudaMemcpyFromDev` | `(Storable a) => CudaDevPtr a -> Ptr a -> CudaT m ()` |
| `cudaMemcpyFromDevBytes` | `CSize -> CudaDevPtr a -> Ptr a -> CudaT m ()` |
| `cudaFree` | `CudaDevPtr a -> CudaT m ()` |

