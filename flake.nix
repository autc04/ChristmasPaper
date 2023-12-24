{
  description = "christmas paper generator";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports =
        [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          packages.ChristmasPaper.root = ./.;

        };
          packages.default = self'.packages.ChristmasPaper;
      };
    };
}
