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
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "home-manager=/nix/var/nix/profiles/per-user/root/channels/home-manager"
      "shabka-path=${shabka-path}"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    binaryCaches = [
      "https://cache.nixos.org/"
      "https://kalbasit.cachix.org"
      "https://yl.cachix.org"
    ];
    binaryCachePublicKeys = [
      "kalbasit.cachix.org-1:cUhsmtACuuKMcExazyXxjhKzXUxf4Suwvt11jsHSfPM="
      "yl.cachix.org-1:Abr5VClgHbNd2oszU+ivr+ujB0Jt2swLo2ddoeSMkm0="
    ];
    trustedUsers = [ "root" "@wheel" ];

    useSandbox = true;

    distributedBuilds = true;
    buildMachines = [{
      hostName = "zeus.home.nasreddine.com";
      sshUser = "yl";
      sshKey = "/yl/private/private-home-files/.ssh/personal/id_rsa";
      system = "x86_64-linux";
      maxJobs = 8;
      speedFactor = 2;
      supportedFeatures = [ ];
      mandatoryFeatures = [ ];
    }];
  };
}
