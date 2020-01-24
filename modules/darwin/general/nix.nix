{ config, pkgs, ... }:

let

  shabka-path = builtins.toPath ./../../..;

in {
  nix = {
    buildCores = 0;
    distributedBuilds = true;
    # useSandbox = true;

    nixPath = [
      "darwin-config=/run/current-system/shabka/hosts/${config.networking.hostName}/configuration.nix"
      "nixpkgs=/run/current-system/nixpkgs"
      "shabka-path=/run/current-system/shabka"
    ];

    binaryCaches = [
      "http://cache.nixos.org"
      "http://yl.cachix.org"
      "http://risson.cachix.org"
    ];

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "yl.cachix.org-1:Abr5VClgHbNd2oszU+ivr+ujB0Jt2swLo2ddoeSMkm0="
      "risson.cachix.org-1:x5ge8Xn+YFlaEqQr3oHhMXxHPYSXbG2k2XFtGqGemwg="
    ];
  };

  system.activationScripts.postActivation.text = ''
    ln -sfn ${pkgs.path} $systemConfig/nixpkgs
    ln -sfn ${shabka-path} $systemConfig/shabka
  '';
}
