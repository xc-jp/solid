{ pkgs, lib, ... }:
{
  # We want mark the tensor-cuda package as unbuildable when cudaSupport is set
  # to false. However, that is not enough to enable building the remaining
  # packages as nix will still evaluate the package causing a failing build of
  # `cudatoolkit` (when allowUnfree is set to false).
  #
  # Hence, we also explicitly remove the package from the list of local haskell
  # packages, this list is usually generated from the stack.yaml file or the
  # cabal.project file depending on how haskell.nix's project function is
  # called.
  packages.solid-cuda.buildable = lib.mkForce pkgs.config.cudaSupport;
  packages.solid-cuda.package.isLocal = lib.mkForce pkgs.config.cudaSupport;
}
