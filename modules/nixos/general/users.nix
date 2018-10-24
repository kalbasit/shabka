{ pkgs, ... }:

{
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
      kalbasit = {
        uid = 2000;

        group = "mine";
        extraGroups = [
          "docker"
          "fuse"
          "libvirtd"
          "networkmanager"
          "vboxusers"
          "video"
          "wheel"
        ];

        shell = pkgs.zsh;
        hashedPassword = "$6$0bx5eAEsHJRxkD8.$gJ7sdkOOJRf4QCHWLGDUtAmjHV/gJxPQpyCEtHubWocHh9O7pWy10Frkm1Ch8P0/m8UTUg.Oxp.MB3YSQxFXu1";
        isNormalUser = true;

        openssh.authorizedKeys.keys = [
          (builtins.readFile (builtins.fetchurl {
            url = "https://github.com/kalbasit.keys";
            sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
          }))
        ];
      };

      root = {
        openssh.authorizedKeys.keys = [
          (builtins.readFile (builtins.fetchurl {
            url = "https://github.com/kalbasit.keys";
            sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
          }))
        ];
      };
    };
  };
}
