{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };

  makeUser = userName: { uid, isAdmin ? false, home ? "/home/${userName}" }: nameValuePair
    userName
    {
      inherit home uid;

      group = "mine";
      extraGroups = [
        "builders"
        "dialout"
        "fuse"
        "users"
        "video"
      ]
      ++ config.shabka.users.groups
      ++ (optionals isAdmin ["wheel"]);

      shell = pkgs.zsh;
      hashedPassword = "$6$2YnxY3Tl$kRj7YZypnB2Od41GgpwYRcn4kCcCE6OksZlKLws0rEi//T/emKWEsUZZ2ZG40eph1bpmjznztav4iKc8scmqc1";
      isNormalUser = true;

      openssh.authorizedKeys.keys = singleton shabka.external.kalbasit.keys;
    };

  makeHM = userName: { uid, isAdmin, home ? "/home/${userName}", ... }: nameValuePair
    userName
    (config.shabka.home-manager.config {
      inherit userName uid isAdmin home;
      nixosConfig = config;
    });

  defaultUsers = {
    risson = { uid = 2000; isAdmin = true; home = "/home/risson"; };
  };

in {
  options.shabka.users = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Enable the management of users and groups.
      '';
    };

    users = mkOption {
      type = types.attrs;
      default = defaultUsers;
      defaultText = ''
        The default users are ${builtins.concatStringsSep " " (builtins.attrNames defaultUsers)}
      '';
      description = ''
        The list of users to create.
      '';
    };

    groups = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        The list of groups to add all users to.
      '';
    };
  };

  config = mkIf (config.shabka.users.enable) {
    users = {
      mutableUsers = false;

      groups = {
        builders = { gid = 1999; };
        mine = { gid = 2000; };
      };

      users = mergeAttrs
        { root = { openssh.authorizedKeys.keys = singleton shabka.external.kalbasit.keys; }; }
        (mapAttrs' makeUser config.shabka.users.users);
    };

    home-manager.users = mapAttrs' makeHM config.shabka.users.users; # XXX: This should be gated by an option
  };
}
