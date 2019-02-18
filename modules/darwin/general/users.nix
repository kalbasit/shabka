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

      gid = 2000;
      isHidden = false;
      shell = pkgs.zsh;
    });

  makeHM = userName: { uid, isAdmin, home ? "/Users/${userName}", ... }: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home;
      darwinConfig = config;
    });

  defaultUsers = {
    yl = { uid = 2000; isAdmin = true; };
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

      groups = {
        mine = { gid = 2000; };

        admin = {
          gid = 80;
          members = ["yl"]; # TODO: This must be computed from the config.mine.users
        };
        staff = {
          gid = 20;
          members = ["yl"]; # TODO: This must be computed from the config.mine.users
        };
      };

      users = (mapAttrs' makeUser config.mine.users);
    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
