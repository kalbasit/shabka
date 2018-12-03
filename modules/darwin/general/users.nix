# TODO(low): follow the same structure as NixOS, maybe with a common location?

{ config, pkgs, lib, ... }:

with lib;

let
  sshKeys = [
    (builtins.readFile (builtins.fetchurl {
      url = "https://github.com/kalbasit.keys";
      sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
    }))
  ];

  makeUser = userName: { uid, isAdmin ? false, home ? "/home/${userName}" }: nameValuePair
    (userName)
    ({
      inherit home uid;

      gid = 2000;

      shell = "${getBin pkgs.zsh}/bin/zsh";
    });

  makeHM = userName: { uid, isAdmin, home, ... }: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home;
      darwinConfig = config;
    });

  defaultUsers = {
    yl = { uid = 2000; isAdmin = true;  home = "/Users/yl"; };
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

      groups = { mine = { gid = 2000; }; };

      users = (mapAttrs' makeUser config.mine.users);

    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
