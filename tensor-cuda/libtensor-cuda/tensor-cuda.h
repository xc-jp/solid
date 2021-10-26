#ifdef __cplusplus
extern "C" {
#endif
#include <stddef.h>
int devMalloc(size_t bytes, void** p);
int devFree(void* devp);
int memcpyToDev(size_t bytes, void* devDst, void* hostSrc);
int memcpyFromDev(size_t bytes, void* hostDst, void* devSrc);
#ifdef __cplusplus
}
#endif
