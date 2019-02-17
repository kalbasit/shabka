{ pkgs, ... }:

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
      "https://cache.nixos.org/"
      "https://yl.cachix.org"
    ];

    binaryCachePublicKeys = [
      "yl.cachix.org-1:Abr5VClgHbNd2oszU+ivr+ujB0Jt2swLo2ddoeSMkm0="
    ];
  };

  system.activationScripts.postActivation.text = ''
    ln -sfn ${pkgs.path} $systemConfig/nixpkgs
    ln -sfn ${shabka-path} $systemConfig/shabka
  '';
}
