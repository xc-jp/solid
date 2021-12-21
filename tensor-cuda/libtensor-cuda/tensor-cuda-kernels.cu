#include <cuda.h>
#include "tensor-cuda.h"

#define checkCudaErr(...)       \
  {                             \
    int err = (int)__VA_ARGS__; \
    if (err)                    \
      return err;               \
  }

int deviceSynchronize() {
  return cudaDeviceSynchronize();
}

int devMalloc(const size_t bytes, void** p) {
  checkCudaErr(cudaMalloc(p, bytes));
  return 0;
}

int devFree(void* devp) {
  checkCudaErr(cudaFree(devp));
  return 0;
}

int memcpyToDev(const size_t bytes, void* devDst, void* hostSrc) {
  checkCudaErr(cudaMemcpy(devDst, hostSrc, bytes, cudaMemcpyHostToDevice));
  return 0;
}

int memcpyFromDev(const size_t bytes,
                               void* hostDst,
                               void* devSrc) {
  checkCudaErr(cudaMemcpy(hostDst, devSrc, bytes, cudaMemcpyDeviceToHost));
  return 0;
}

int getDevice(int* device) {
  checkCudaErr(cudaGetDevice(device));
  return 0;
}
