{ config, pkgs, lib, ... }:

let
  system-path = builtins.toPath ../..;
in

{
  nix = {
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    useSandbox = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
    nixPath = [
      "nixpkgs=${system-path}/external/nixpkgs"
      "nixos-config=${system-path}/machines/${config.networking.hostName}/configuration.nix"
    ];
  };
}
