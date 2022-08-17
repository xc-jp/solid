# `solid`

A library for specifying Tensors, their Shapes and their contained Element types.

## Modules

`Data.Solid.Array` exposes most of what one would need.

In the most general case, `Tensor v a` is `v a` with dimensions, as is defined in `Data.Solid.Common`:

```haskell
data Tensor v a = Tensor
  { tensorDims :: !Dims,
    tensorData :: !(v a)
  }
```

The dimension type `Dims` is a list of positive numbers (See `Data.Solid.Positive`).

`Data.Solid.Vector` provides some utilities when `v` is a variant of `Vector` from [the `vector` package](https://hackage.haskell.org/package/vector).

`Data.Solid.Array` defines dynamic arrays, where `a` is either `Float` (32-bit floating number) or `Int32` (32-bit integer).

`Data.Solid.Approx` defines an approximate equality relation on dynamic vector arrays.

