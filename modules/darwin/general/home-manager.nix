{ config, pkgs, lib, ... }:

with lib;

let
  homeManager =
    let
      nixpkgs = import ../../../external/nixpkgs-stable.nix;
      pkgs = import nixpkgs {
        config = {};
        overlays = [];
      };
    in import ../../../external/home-manager.nix {
      inherit (pkgs) fetchpatch runCommand;
    };
in {
  imports = [
    (import "${homeManager}/nix-darwin")
  ];

  options.mine.home-manager.config = mkOption {
    default = { name, uid, isAdmin, darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}

