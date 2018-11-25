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

      group = "mine";
      extraGroups = [
        "dialout"
        "fuse"
        "libvirtd"
        "users"
        "video"
      ]
      ++ config.mine.userGroups
      ++ (optionals isAdmin ["wheel"]);

      shell = pkgs.zsh;
      hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
      isNormalUser = true;

      openssh.authorizedKeys.keys = sshKeys;
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

  options.mine.userGroups = mkOption {
    type = types.listOf types.str;
    default = [];
    description = ''
      The list of groups to add all users to.
    '';
  };

  config = {
    # set the initial password of the root user
    security.initialRootPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";

    users = {
      mutableUsers = false;

      groups = { mine = { gid = 2000; }; };

      users = mergeAttrs
        { root = { openssh.authorizedKeys.keys = sshKeys; }; }
        (mapAttrs' makeUser config.mine.users);
    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
