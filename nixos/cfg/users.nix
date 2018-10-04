{ config, pkgs, ...}:

{
  imports = [
    (import <home-manager> {}).nixos
  ];

  # configure the kalbasit user using home-manager
  home-manager.users.kalbasit = import ../../home/cfg;

  # enable zsh
  programs.zsh.enable = true;

  # set the default to zsh for all users
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # do not allow the users to be mutated
  users.mutableUsers = false;

  # define the root user
  users.users.root = {
    openssh.authorizedKeys.keys = [
      (builtins.readFile (builtins.fetchurl {
        url = "https://github.com/kalbasit.keys";
        sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
      }))
    ];
  };

  # define the users
  users.users.kalbasit = {
    extraGroups = [
      "docker"
      "fuse"
      "libvirtd"
      "networkmanager"
      "vboxusers"
      "video"
      "wheel"
    ];

    hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
    isNormalUser = true;
    uid = 1026;

    openssh.authorizedKeys.keys = [
      (builtins.readFile (builtins.fetchurl {
        url = "https://github.com/kalbasit.keys";
        sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
      }))
    ];
  };
}
