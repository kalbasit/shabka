{ config, pkgs, lib, ... }:

with lib;

let

  shabka-path = builtins.toPath ./../../..;

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
      "nixos-config=/run/current-system/shabka/hosts/${config.networking.hostName}/configuration.nix"
      "nixpkgs=/run/current-system/nixpkgs"
      "shabka-path=/run/current-system/shabka"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    binaryCaches = [
      "https://cache.nixos.org/"
      "https://yl.cachix.org"
    ];
    binaryCachePublicKeys = [
      "yl.cachix.org-1:Abr5VClgHbNd2oszU+ivr+ujB0Jt2swLo2ddoeSMkm0="
    ];
    trustedUsers = [ "root" "@wheel" "@builders"];

    useSandbox = true;

    distributedBuilds = true;
  };


  # Pin the nixpkgs under /run/current-system/nixpkgs
  # Alternatively, this can be via the activationScripts
  # system.activationScripts.pinnixpkgs = ''
  #   echo "setting up /run/current-nixpkgs..."
  #   ln -sfn ${pinnedNixpkgs} /run/current-nixpkgs
  # '';
  system.extraSystemBuilderCmds = ''
    ln -sv ${pkgs.path} $out/nixpkgs
    ln -sv ${shabka-path} $out/shabka
  '';
}
