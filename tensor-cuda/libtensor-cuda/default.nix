{ stdenv, cmake, cudart, cudatoolkit, addOpenGLRunpath }:
stdenv.mkDerivation {
  src = ./.;
  buildInputs = [ cmake cudart cudatoolkit ];
  nativeBuildInputs = [ addOpenGLRunpath ];
  name = "libtensor-cuda";
  # TODO explain this voodoo
  postFixup = ''
    addOpenGLRunpath $out/lib/libtensor-cuda.so
  '';
}
