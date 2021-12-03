#ifndef TENSOR_CUDA_H
#define TENSOR_CUDA_H
#include <cstddef>
namespace tensor_cuda {
int devMalloc(const size_t bytes, void** p);
int devFree(void* devp);
int memcpyToDev(const size_t bytes, void* devDst, void* hostSrc);
int memcpyFromDev(const size_t bytes, void* hostDst, void* devSrc);
int getDevice(int* device);
}  // namespace tensor_cuda
#endif
