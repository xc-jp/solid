#ifndef TENSOR_CUDA_H
#define TENSOR_CUDA_H
namespace tensor_cuda {
  int devMalloc(size_t bytes, void **p);
  int devFree(void *devp);
  int memcpyToDev(size_t bytes, void *devDst, void *hostSrc);
  int memcpyFromDev(size_t bytes, void *hostDst, void *devSrc);
}
#endif
