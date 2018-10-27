{ config, pkgs, lib, ... }:

let

  system-path = builtins.toPath ./../../..;

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
      "system-path=${system-path}"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    binaryCaches = [
      "https://cache.nixos.org/"
      "https://kalbasit.cachix.org"
    ];
    binaryCachePublicKeys = [
      "kalbasit.cachix.org-1:cUhsmtACuuKMcExazyXxjhKzXUxf4Suwvt11jsHSfPM="
    ];
    trustedUsers = [ "root" "kalbasit" ];

    useSandbox = true;
  };
}
