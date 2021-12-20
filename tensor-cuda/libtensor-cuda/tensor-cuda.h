#ifdef __cplusplus
extern "C" {
#endif
#include <stddef.h>

// device
int getDevice(int* device);
int deviceSynchronize();

// memory
int devMalloc(const size_t bytes, void** p);
int devFree(void* devp);
int memcpyToDev(const size_t bytes, void* devDst, void* hostSrc);
int memcpyFromDev(const size_t bytes, void* hostDst, void* devSrc);
#ifdef __cplusplus
}
#endif
