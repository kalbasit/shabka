{ config, pkgs, lib, ... }:

let
  system-path = builtins.toPath ./../..;

  pinned-nixos-hardware = pkgs.callPackage ../../external/nixos-hardware.nix {};
  pinned-nixpkgs        = pkgs.callPackage ../../external/nixpkgs.nix {};
  pinned-home-manager   = pkgs.callPackage ../../external/home-manager.nix {};
in {
  nix = {
    autoOptimiseStore = true;
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;

    extraOptions = ''
      auto-optimise-store = true
    '';

    nixPath = [
      # externals
      "home-manager=${pinned-home-manager}"
      "nixos-hardware=${pinned-nixos-hardware}"
      "nixpkgs=${pinned-nixpkgs}"

      # system
      "system-path=${system-path}"
      "nixpkgs-overlays=${system-path}/overlays"

      # machine-specific
      "nixos-config=${system-path}/nixos/machines/${config.networking.hostName}/configuration.nix"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    useSandbox = true;
  };
}
