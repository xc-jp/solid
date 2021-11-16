{ pkgs, lib, ... }:
{
  # TODO explanations
  packages.tensor-cuda.buildable = lib.mkForce pkgs.config.cudaSupport;
  packages.tensor-cuda.package.isLocal = lib.mkForce pkgs.config.cudaSupport;
}
