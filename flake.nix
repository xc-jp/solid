{
  description = "solid";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        nonlinear-src = pkgs.fetchFromGitHub {
          owner = "xc-jp";
          repo = "nonlinear";
          rev = "5be5da6375209e75ddb46a3e5e9e315a9841c573";
          sha256 = "sha256-4yt+gJQcJ7YTVoqkjlTk8NDwLpqw8Fj95ICmRWMqyFE=";
        };
        overlay = final: prev: {
          hsPkgs = final.haskell.packages.ghc8107.extend (hfinal: hprev: {
            nonlinear = hfinal.callCabal2nix "nonlinear" nonlinear-src { };
            solid = hfinal.callCabal2nix "solid" ./solid { };
            solid-cuda = hfinal.callCabal2nix "solid-cuda" ./solid-cuda { };
          });
          cudart = (final.callPackage
            "${nixpkgs}/pkgs/development/compilers/cudatoolkit"
            { }).cudatoolkit_11_2;
        };
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
              "cudatoolkit"
            ];
          };
          overlays = [
            overlay
          ];
        };
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
            pkgs.cudart
          ];
        };

        packages = {
          solid = pkgs.hsPkgs.solid;
          solid-cuda = pkgs.hsPkgs.solid-cuda;
        };

        defaultPackage = pkgs.hsPkgs.solid;
      }
    );
}
