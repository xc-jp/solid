#include "tensor-cuda-kernels.cuh"
#include "tensor-cuda.h"

// device
int getDevice(int* device) {
  return tensor_cuda::getDevice(device);
}
int deviceSynchronize() {
  return tensor_cuda::deviceSynchronize();
}

// memory
int devMalloc(const size_t bytes, void** p) {
  return tensor_cuda::devMalloc(bytes, p);
}
int devFree(void* devp) {
  return tensor_cuda::devFree(devp);
}
int memcpyToDev(const size_t bytes, void* devDst, void* hostSrc) {
  return tensor_cuda::memcpyToDev(bytes, devDst, hostSrc);
}
int memcpyFromDev(const size_t bytes, void* hostDst, void* devSrc) {
  return tensor_cuda::memcpyFromDev(bytes, hostDst, devSrc);
}
