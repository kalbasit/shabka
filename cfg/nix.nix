{ config, pkgs, lib, ... }:

let
  system-path = builtins.toPath ./..;
in

{
  nix = {
    autoOptimiseStore = true;
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;

    extraOptions = ''
      auto-optimise-store = true
    '';

    nixPath = [
      "nixos-config=${system-path}/machines/${config.networking.hostName}/configuration.nix"
      "nixos-hardware=${system-path}/external/nixos-hardware"
      "nixpkgs-overlays=${system-path}/overlays"
      "nixpkgs=${system-path}/external/nixpkgs"
      "system-path=${system-path}"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    useSandbox = true;
  };
}
