{ config, pkgs, lib, ... }:

let
  dotfiles-path = builtins.path { path = ../..; name = "dotfiles"; };
in

{
  nix = {
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    useSandbox = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
    nixPath = [
      "nixpkgs=/home/kalbasit/code/personal/base/src/github.com/kalbasit/system/external/nixpkgs"
      # "nixos-config=${dotfiles-path}/machines/${config.networking.hostName}/configuration.nix"
      "nixos-config=/home/kalbasit/code/personal/base/src/github.com/kalbasit/system/machines/${config.networking.hostName}/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };
}
