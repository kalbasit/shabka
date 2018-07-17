{ config, pkgs, ...}:

{
  # enable zsh
  programs.zsh.enable = true;

  # set the default to zsh for all users
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # define the users
  users.extraUsers.kalbasit = {
    extraGroups = [
      "docker"
      "fuse"
      "libvirtd"
      "networkmanager"
      "sway"
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
        sha256 = "439dea6077640c229dcaa2a2849c57424b8d7731ecc3bd4fc4ca11bb1f98cde2";
      }))
    ];
  };
}
