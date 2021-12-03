#include "tensor-cuda.h"
#include "tensor-cuda-kernels.cuh"
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
int getDevice(int* device) {
  return tensor_cuda::getDevice(device);
}
