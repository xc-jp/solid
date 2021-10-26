#ifndef TENSOR_CUDA_H
#define TENSOR_CUDA_H
namespace tensor_cuda {
int devMalloc(const size_t bytes, void** p);
int devFree(void* devp);
int memcpyToDev(const size_t bytes, void* devDst, void* hostSrc);
int memcpyFromDev(const size_t bytes, void* hostDst, void* devSrc);
}  // namespace tensor_cuda
#endif
