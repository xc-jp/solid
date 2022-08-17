cmake_minimum_required(VERSION 3.8 FATAL_ERROR)
project(libsolid-cuda_project LANGUAGES CUDA CXX)
set(cxx_flags -Wall -Wextra -pedantic -Werror -O2)
set(cuda_flags -arch=sm_50
      -gencode=arch=compute_50,code=sm_50
      -gencode=arch=compute_52,code=sm_52
      -gencode=arch=compute_53,code=sm_53
      -gencode=arch=compute_60,code=sm_60
      -gencode=arch=compute_62,code=sm_62
      -gencode=arch=compute_70,code=sm_70
      -gencode=arch=compute_70,code=compute_70)
set(VERBOSE ON)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CUDA_VERBOSE_BUILD ON)
if (MSVC)
    add_compile_options(/W4 /WX)
else()
    add_compile_options(
      "$<$<COMPILE_LANGUAGE:CXX>:${cxx_flags}>"
      "$<$<COMPILE_LANGUAGE:CUDA>:${cuda_flags}>"
      )
endif()
add_library(solid-cuda SHARED solid-cuda-kernels.cu)
target_compile_features(solid-cuda PUBLIC cxx_std_11 cuda_std_11)

set_target_properties(solid-cuda PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    # The arch and gencode flags aren't properly propagated to the nvida linker
    # (nvlink) when using separable compilation. Since all functions are
    # internal to this library it should be fine to not use separable
    # compilation as they are all accessible in the same compilation unit
    # (ops.cu).
    CUDA_SEPARABLE_COMPILATION OFF
    CUDA_ARCHITECTURES 50 52 60 61 70)

target_link_libraries(solid-cuda "${CUDA_LIBRARIES}")
install(TARGETS solid-cuda DESTINATION lib)
install(FILES solid-cuda.h DESTINATION include)
