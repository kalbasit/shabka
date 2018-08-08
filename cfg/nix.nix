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
      "system-path=${system-path}"
      "nixpkgs=${system-path}/external/nixpkgs"
      "nixos-config=${system-path}/machines/${config.networking.hostName}/configuration.nix"
      "nixos-hardware=${system-path}/external/nixos-hardware"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    useSandbox = true;
  };
}
