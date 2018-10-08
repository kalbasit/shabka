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
      # system
      "system-path=${system-path}"

      # machine-specific
      "nixos-config=${system-path}/hosts/${config.networking.hostName}/configuration.nix"
    ];

    optimise = {
      automatic = true;
      dates = [ "12:00" ];
    };

    useSandbox = true;
  };
}
