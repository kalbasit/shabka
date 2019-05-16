{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  nix = {
    autoOptimiseStore = true;
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    distributedBuilds = true;
    useSandbox = true;

    extraOptions = ''
      auto-optimise-store = true
    '';

    nixPath = [
      "nixos-config=/run/current-system/shabka/hosts/${config.networking.hostName}/configuration.nix"
      "nixpkgs=/run/current-system/nixpkgs"
      "shabka=/run/current-system/shabka"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    binaryCaches = [ #TODO: allow for more binary caches
      "https://cache.nixos.org/"
    ];

    trustedUsers = [
      "root" "@wheel" "@builders"
    ];
  };


  # Pin the nixpkgs under /run/current-system/nixpkgs
  # Alternatively, this can be via the activationScripts
  # system.activationScripts.pinnixpkgs = ''
  #   echo "setting up /run/current-nixpkgs..."
  #   ln -sfn ${pinnedNixpkgs} /run/current-nixpkgs
  # '';
  system.extraSystemBuilderCmds = ''
    ln -sfn ${pkgs.path} $out/nixpkgs
    ln -sfn ${shabka.path} $out/shabka
  '';
}
