# TODO(low): follow the same structure as NixOS, maybe with a common location?

{ config, pkgs, lib, ... }:

with lib;

let
  sshKeys = [
    (builtins.readFile (import ../../../external/kalbasit-keys.nix))
  ];

  makeUser = userName: { uid, isAdmin ? false, home ? "/home/${userName}" }: nameValuePair
    (userName)
    ({
      inherit home uid;

      gid = 20;

      shell = "${getBin pkgs.zsh}/bin/zsh";
    });

  makeHM = userName: { uid, isAdmin, home, ... }: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home;
      darwinConfig = config;
    });

  defaultUsers = {
    yl = { uid = 501; isAdmin = true;  home = "/Users/yl"; };
  };

in {
  options.mine.users = mkOption {
    type = types.attrs;
    default = defaultUsers;
    defaultText = ''
      The default users are ${builtins.concatStringsSep " " (builtins.attrNames defaultUsers)}
    '';
    description = ''
      The list of users to create.
    '';
  };

  config = {
    users = {
      knownUsers = builtins.attrNames config.mine.users;
      knownGroups = [ "mine" ];

      users = (mapAttrs' makeUser config.mine.users);

    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
