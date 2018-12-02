{ config, pkgs, lib, ... }:

with lib;

let
  homeManager = import ../../../external/home-manager.nix {
    pkgs = (import <nixpkgs> {});
    inherit (import ../../../util) assertMsg;
  };
in {
  imports = [
    (import homeManager {}).nix-darwin

    # load the following when running a VM
    # ("${builtins.fetchTarball https://github.com/rycee/home-manager/archive/nixos-module-user-pkgs.tar.gz}/nix-darwin")
  ];

  options.mine.home-manager.config = mkOption {
    default = { name, uid, isAdmin, darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}

