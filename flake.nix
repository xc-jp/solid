{
  description = "solid";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nonlinear-src = {
    url = "github:xc-jp/nonlinear";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, nonlinear-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          hsPkgs = final.haskell.packages.ghc8107.extend (hfinal: hprev: {
            nonlinear = hfinal.callCabal2nix "nonlinear" nonlinear-src { };
            solid = hfinal.callCabal2nix "solid" ./solid { };
            solid-cuda = final.callCabal2nix "solid-cuda" ./solid-cuda/solid-cuda { };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          config.cudaSupport = true;
          overlays = [
            overlay
          ];
        };
        inherit (pkgs.cudaPackages) cudatoolkit;
        inherit (pkgs.linuxPackages) nvidia_x11;
      in
      {
        devShell = pkgs.hsPkgs.shellFor {
          packages = ps: [ ps.solid ];
          nativeBuildInputs = [
            pkgs.bashInteractive
            pkgs.ormolu
            pkgs.hlint
            pkgs.hsPkgs.haskell-language-server
            pkgs.cabal-install
            pkgs.hsPkgs.cabal-fmt
            nvidia_x11
            cudatoolkit
            ];
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [cudatoolkit nvidia_x11]}
            echo "cudatoolkit ${cudatoolkit.version}"
            echo "nvidia_x11 ${nvidia_x11.version}"
          '';
        };
      }
    );
}
