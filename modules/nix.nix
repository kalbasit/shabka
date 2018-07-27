{ config, pkgs, lib, ... }:

let
  system-path = builtins.toPath /code/personal/base/src/github.com/kalbasit/system;
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
