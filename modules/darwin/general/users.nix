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

      shell = pkgs.zsh;
    });

  makeHM = userName: { uid, isAdmin ? false, ... }: nameValuePair
    (userName)
    (config.mine.home-manager.config {
      inherit userName uid isAdmin;
      nixosConfig = config;
    });

  defaultUsers = {
    yl              = { uid = 2000; isAdmin = true;  home = "/yl"; };
    yl_opensource   = { uid = 2002; isAdmin = false; home = "/yl/opensource"; };
    yl_presentation = { uid = 2003; isAdmin = false; home = "/yl/presentation"; };
    yl_publica      = { uid = 2016; isAdmin = false; home = "/yl/publica"; };
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

    # TODO: https://github.com/rycee/home-manager/pull/240
    # home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
