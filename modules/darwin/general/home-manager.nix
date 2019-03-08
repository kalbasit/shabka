{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };

  makeHM = userName: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      nixosConfig = config;
    });
in {
  imports = [
    (import "${shabka.external.home-manager.path}/nix-darwin")
  ];

  options.mine.home-manager.config = mkOption {
    default = { darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
