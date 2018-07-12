# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # Include the results of the hardware scan.
  imports = [
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.editor = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # enable build on Sandbox
  nix.useSandbox = true;

  # Define your hostname.
  networking.hostName = "cratos";

  # Enable the network manager, it makes life easier
  networking.networkmanager.enable = true;

  # Add the extra hosts
  networking.extraHosts = ''
    127.0.0.1 api.publica.dev console.publica.dev home.publica.dev ctrl.publica.dev js.publica.dev rewriter.publica.dev publica.dev
  '';

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    defaultLocale = "en_US.UTF-8";
    consoleKeyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # core of my tools
    git
    curl
    neovim # TODO: use the Neovim package I create in my .nixpkgs
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # enable zsh
  programs.zsh.enable = true;

  # enable Sway
  programs.sway.enable = true;

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

  # List services that you want to enable:

  # Give people part of the video group access to adjust the backlight
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Support for my workflow. This can be removed once SWM v2 lands.
  services.openssh.extraConfig = ''
    Match User kalbasit
      AcceptEnv ACTIVE_PROFILE
      AcceptEnv ACTIVE_STORY
  '';

  # Enable fwupd
  services.fwupd.enable = true;

  # Install and enable Keybase
  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # TODO TODO TODO: remove this once it's all setup
  security.sudo.wheelNeedsPassword = false;

  # hide process information of other users when running non-root
  security.hideProcessInformation = true;

  # update the system periodically to receive security updates as they become available
  system.autoUpgrade.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.nixos.stateVersion = "18.03"; # Did you read the comment?
}
