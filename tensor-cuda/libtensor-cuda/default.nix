{ pkgs ? import ../../nix { } }:
let
  src = ./.;
  libtensor-cuda = with pkgs; stdenv.mkDerivation {
    src = src;
    buildInputs = [ cmake cudart cudatoolkit ];
    name = "libtensor-cuda";
  };
  sourceTarball = pkgs.runCommand "libtensor-cuda-source" { } ''
    ${pkgs.gnutar}/bin/tar czf $out ${src}/*
  '';
in
{ inherit libtensor-cuda sourceTarball; }
