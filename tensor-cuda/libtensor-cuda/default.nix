{ stdenv, cmake, cudart, cudatoolkit, addOpenGLRunpath }:
stdenv.mkDerivation {
  src = ./.;
  buildInputs = [ cmake cudart cudatoolkit ];
  nativeBuildInputs = [ addOpenGLRunpath ];
  name = "libtensor-cuda";
  # To run any executable that depends on libtensor-cuda.so we need the
  # libcuda.so nvidia **driver** to be available to the linker at runtime. On
  # NixOS that driver is placed under /run/opengl-driver/lib and the below
  # fixup hook adds this path to the runtime-path of libtensor-cuda.so. On
  # non-NixOS systems (like inside docker containers) we can still
  # LD_LIBRARY_PATH or LD_PRELOAD the libcuda.so driver and this should not
  # interfere with that unless an incompatible driver version is placed under
  # /run/opengl-driver/lib (very unlikely).
  postFixup = ''
    addOpenGLRunpath $out/lib/libtensor-cuda.so
  '';
}
