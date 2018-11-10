# TODO(high): follow the same structure as NixOS, maybe with a common location?

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

      # group = "mine";
      # extraGroups = [
      #   "dialout"
      #   "docker"
      #   "fuse"
      #   "libvirtd"
      #   "networkmanager"
      #   "users"
      #   "vboxusers"
      #   "video"
      # ] ++ (if isAdmin then ["wheel"] else []);

      shell = pkgs.zsh;
      # hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
      # isNormalUser = true;

      # openssh.authorizedKeys.keys = sshKeys;
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
    # set the initial password of the root user
    # security.initialRootPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";

    users = {
      # mutableUsers = false;

      groups = { mine = { gid = 2000; }; };

      users = mergeAttrs
        {
          # root = { openssh.authorizedKeys.keys = sshKeys; };
        }
        (mapAttrs' makeUser config.mine.users);
    };

    # home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
