{ pkgs, ... }:

let
  sshKeys = [
    (builtins.readFile (builtins.fetchurl {
      url = "https://github.com/kalbasit.keys";
      sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
    }))
  ];

  makeUser = uid: admin: {
    inherit uid;

    group = "mine";
    extraGroups = [
      "docker"
      "fuse"
      "libvirtd"
      "networkmanager"
      "vboxusers"
      "video"
    ] ++ (if admin then ["wheel"] else []);

    shell = pkgs.zsh;
    hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
    isNormalUser = true;

    openssh.authorizedKeys.keys = sshKeys;
  };

in {
  # set the initial password of the root user
  security.initialRootPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";

  users = {
    mutableUsers = false;

    groups = {
      mine = {
        gid = 2000;
      };
    };

    users = {
      yl = makeUser 2000 true;
      yl_admin = makeUser 2001 false;
      yl_opensource = makeUser 2003 false;
      yl_publica = makeUser 2002 false;

      root = { openssh.authorizedKeys.keys = sshKeys; };
    };
  };
}
