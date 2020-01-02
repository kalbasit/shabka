{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };

  makeHM = userName: nameValuePair
    (userName)
    (config.shabka.home-manager.config {
      nixosConfig = config;
    });
in {
  imports = [
    (import "${shabka.external.home-manager.release-unstable.path}/nix-darwin")
  ];

  options.shabka.home-manager.config = mkOption {
    default = { darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
