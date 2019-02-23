# TODO(low): follow the same structure as NixOS, maybe with a common location?

{ config, pkgs, lib, ... }:

with lib;

let
  sshKeys = [
    (builtins.readFile (import ../../../external/kalbasit-keys.nix))
  ];

  makeUser = userName: { uid, isAdmin ? false, home ? "/Users/${userName}" }: nameValuePair
    (userName)
    ({
      inherit home uid;

      gid = 80;
      isHidden = false;
      shell = "${getBin pkgs.zsh}/bin/zsh";
    });

  makeHM = userName: { }: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home;
      darwinConfig = config;
    });

  defaultUsers = {
    yl = {};
  };

in {
  users.users = (mapAttrs' makeUser defaultUsers);
  home-manager.users = mapAttrs' makeHM defaultUsers;
}
