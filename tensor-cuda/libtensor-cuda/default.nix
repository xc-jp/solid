{ stdenv, cmake, cudart, cudatoolkit }:
stdenv.mkDerivation {
  src = ./.;
  buildInputs = [ cmake cudart cudatoolkit ];
  name = "libtensor-cuda";
}
