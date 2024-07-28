{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, haskellNix, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system :
      let
        overlays = [ haskellNix.overlay ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc910";

          shell.tools = {
            ghcid = {};
            haskell-language-server = {};
          };
        };

        flake = project.flake {};
      in
        pkgs.lib.recursiveUpdate flake {
          packages.default = flake.packages."interval-map:lib:interval-map";
        });
}
