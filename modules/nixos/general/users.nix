{ config, pkgs, shabka ? import <shabka> { inherit pkgs; }, lib, ... }:

with lib;

let
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
      ++ config.mine.userGroups
      ++ (optionals isAdmin ["wheel"]);

      shell = pkgs.zsh;
      hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
      isNormalUser = true;

      openssh.authorizedKeys.keys = singleton shabka.external.kalbasit.keys;
    };

  makeHM = userName: { uid, isAdmin, home ? "/home/${userName}", ... }: nameValuePair
    userName
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home;
      nixosConfig = config;
    });

  defaultUsers = {
    yl              = { uid = 2000; isAdmin = true;  home = "/yl"; };
    yl_opensource   = { uid = 2002; isAdmin = false; home = "/yl/opensource"; };
    yl_presentation = { uid = 2003; isAdmin = false; home = "/yl/presentation"; };
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
    # XXX: This is now obselete.
    # https://github.com/NixOS/nixpkgs/blob/63a09881b674e35a7e7a64951cd4b0f7e58be685/nixos/modules/config/users-groups.nix#L476-L482
    security.initialRootPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";

    users = {
      mutableUsers = false;

      groups = {
        builders = { gid = 1999; };
        mine = { gid = 2000; };
      };

      users = mergeAttrs
        { root = { openssh.authorizedKeys.keys = singleton shabka.external.kalbasit.keys; }; }
        (mapAttrs' makeUser config.mine.users);
    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
