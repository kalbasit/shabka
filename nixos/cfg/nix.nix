{ config, pkgs, lib, ... }:

let
  system-path = builtins.toPath ./../..;

  nixos-hardware-version = lib.importJSON ../../external/nixos-hardware-version.json;
  pinned-nixos-hardware = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixos-hardware";
    inherit (nixos-hardware-version) rev sha256;
  };


  nixpkgs-version = lib.importJSON ../../external/nixpkgs-version.json;
  pinned-nixpkgs = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs-version) rev sha256;
  };

  home-manager-version = lib.importJSON ../../external/home-manager-version.json;
  pinned-home-manager = pkgs.fetchFromGitHub {
    owner = "rycee";
    repo = "home-manager";
    inherit (home-manager-version) rev sha256;
  };

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
