#include <cuda.h>
#include <cstdio>
#include "tensor-cuda-kernels.cuh"

#define checkCudaErr(...)       \
  {                             \
    int err = (int)__VA_ARGS__; \
    if (err)                    \
      return err;               \
  }

int tensor_cuda::devMalloc(const size_t bytes, void** p) {
  checkCudaErr(cudaMalloc(p, bytes));
  return 0;
}

int tensor_cuda::devFree(void* devp) {
  checkCudaErr(cudaFree(devp));
  return 0;
}

int tensor_cuda::memcpyToDev(const size_t bytes, void* devDst, void* hostSrc) {
  checkCudaErr(cudaMemcpy(devDst, hostSrc, bytes, cudaMemcpyHostToDevice));
  return 0;
}

int tensor_cuda::memcpyFromDev(const size_t bytes,
                               void* hostDst,
                               void* devSrc) {
  checkCudaErr(cudaMemcpy(hostDst, devSrc, bytes, cudaMemcpyDeviceToHost));
  return 0;
}
