{ pkgs ? import ../../nix { } }:
let
  src = ./.;
  libtensor-cuda = with pkgs; stdenv.mkDerivation {
    src = src;
    buildInputs = [ cmake cudart cudatoolkit ];
    name = "tensor-cuda";
  };
  sourceTarball = pkgs.runCommand "tensor-cuda-source" { } ''
    ${pkgs.gnutar}/bin/tar czf $out ${src}/*
  '';
in
{ inherit libtensor-cuda sourceTarball; }
