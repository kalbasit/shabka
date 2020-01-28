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

    binaryCaches = [
      "http://cache.nixos.org"
      "http://yl.cachix.org"
      "http://risson.cachix.org"
      "http://hercules-ci.cachix.org"
    ];

    binaryCachePublicKeys = [
      "yl.cachix.org-1:Abr5VClgHbNd2oszU+ivr+ujB0Jt2swLo2ddoeSMkm0="
      "risson.cachix.org-1:x5ge8Xn+YFlaEqQr3oHhMXxHPYSXbG2k2XFtGqGemwg="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
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
    ln -sfn ${cleanSource pkgs.path} $out/nixpkgs
    ln -sfn ${cleanSource shabka.path} $out/shabka
  '';
}
