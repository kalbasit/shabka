# TODO(low): follow the same structure as NixOS, maybe with a common location?

{ config, pkgs, lib, ... }:

with lib;

let
  sshKeys = [
    (builtins.readFile (import ../../../external/kalbasit-keys.nix))
  ];

  makeUser = userName: { }: nameValuePair
    userName
    {};

  makeHM = userName: { }: nameValuePair
    userName
    (config.mine.home-manager.config {
      darwinConfig = config;
    });

  defaultUsers = {
    yl = {};
  };

in {
  users.users = (mapAttrs' makeUser defaultUsers);
  home-manager.users = mapAttrs' makeHM defaultUsers;
}
