#include "memory.h"
#include "tensorcuda.cuh"
int devMalloc(size_t bytes, void** p) {
  return tensor_cuda::devMalloc(bytes, p);
}
int devFree(void* devp) {
  return tensor_cuda::devFree(devp);
}
int memcpyToDev(size_t bytes, void* devDst, void* hostSrc) {
  return tensor_cuda::memcpyToDev(bytes, devDst, hostSrc);
}
int memcpyFromDev(size_t bytes, void* hostDst, void* devSrc) {
  return tensor_cuda::memcpyFromDev(bytes, hostDst, devSrc);
}
