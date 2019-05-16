{ config, pkgs, lib, ... }:

with lib;

let
  makeUser = userName: { uid, isAdmin ? false, home ? "/home/${userName}", sshKeys ? [] }: nameValuePair
    userName
    {
      inherit home uid;

      group = "mine";
      extraGroups = [
        "builders"
        "fuse"
        "users"
        "video"
      ]
      ++ config.mine.userGroups
      ++ (optionals isAdmin ["wheel"]);

      shell = pkgs.zsh;
      initialPassword = "${userName}";
      isNormalUser = true;

      openssh.authorizedKeys.keys = sshKeys;
    };

  makeHM = userName: { uid, isAdmin, home ? "/home/${userName}", sshKeys ? [], ... }: nameValuePair
    userName
    (config.mine.home-manager.config {
      inherit userName uid isAdmin home sshKeys;
      nixosConfig = config;
    });

  defaultUsers = { };

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

  options.mine.rootSshKeys = mkOption { #TODO: do we really want to do it like this?
    type = type.listOf types.str;
    default = [];
    description = ''
      Ssh keys allowed to login as root.
    '';
  };

  config = {

    users = {
      mutableUsers = true;  # Allow for password changes. Otherwise, we could pass the hashedPassword as an argument of the makeUser function above

      groups = {
        builders = { gid = 1999; };
        mine = { gid = 2000; };
      };

      users = mergeAttrs
        { root = { openssh.authorizedKeys.keys = config.mine.rootSshKeys; }; }
        (mapAttrs' makeUser config.mine.users);
    };

    home-manager.users = mapAttrs' makeHM config.mine.users;
  };
}
