{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
  release = builtins.getEnv "RELEASE";

  makeHM = userName: nameValuePair
    (userName)
    (config.shabka.home-manager.config {
      nixosConfig = config;
    });
in {
  imports = [
    (import "${shabka.external.home-manager."${release}".path}/nix-darwin")
  ];

  options.shabka.home-manager.config = mkOption {
    default = { darwinConfig }: {...}: {};
    description = ''
      Function that returns the Home Manager configuration.
    '';
  };
}
