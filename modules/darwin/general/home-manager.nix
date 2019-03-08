{ config, pkgs, lib, ... }:

with lib;

let
  makeHM = userName: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      nixosConfig = config;
    });
in {
  imports = [
    (import "${pkgs.shabka.external.home-manager.path}/nix-darwin")
  ];

  options.mine.home-manager.config = mkOption {
    default = { darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
