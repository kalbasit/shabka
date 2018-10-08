{
  imports = [
    ../../modules/nixos

    ./hardware-configuration.nix
    ./home.nix
  ];

  networking.hostName = "hades";

  mine.printing.enable = true;
  mine.workstation.enable = true;
  mine.workstation.publica.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}

# {
#   # Include the results of the hardware scan.
#   imports = [
#     <nixos-hardware/common/cpu/intel>
#     <nixos-hardware/common/pc/laptop>
#     <nixos-hardware/common/pc/laptop/ssd>
#
#     ./hardware-configuration.nix
#
#     ../../cfg/common.nix
#     ../../cfg/desktop.nix
#     ../../cfg/docker.nix
#     ../../cfg/redshift.nix
#     ../../cfg/virtualbox.nix
#
#     ../../cfg/printers.nix
#
#     ../../cfg/publica.nix
#
#     ../../cfg/snapper.nix
#   ] ++ (if builtins.pathExists /private then [
#     ../../cfg/openvpn/client/nasreddine/hades.nix
#   ] else []);
#
#   # Enable Tmux
#   mine.tmux.enable = true;
#
#   # Enable GnuPG support
#   mine.gnupg.enable = true;
#
#   # Enable VirtualBox and Docker virtualisation services.
#   mine.virtualisation.docker.enable = true;
#   mine.virtualisation.virtualbox.enable = true;
#
#   # boot the latest kernel
#   boot.kernelPackages = pkgs.linuxPackages_latest;
#
#   # select a console font
#   i18n.consoleFont = "Lat2-Terminus16";
#   boot.earlyVconsoleSetup = true;
#
#   # put /tmp on tmpfs
#   boot.tmpOnTmpfs = true;
#
#   # List services that you want to enable:
#
#   # enable nix-serve
#   services.nix-serve = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/hades.nasreddine.com.key) {
#     enable = true;
#     secretKeyFile = "/private/network-secrets/nix/caches/hades.nasreddine.com.key";
#   };
#   networking.firewall.allowedTCPPorts = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/hades.nasreddine.com.key) [ 5000 ];
#
#   # The power button should trigger suspend
#   services.logind.extraConfig = ''
#     HandlePowerKey=suspend
#   '';
#
#   # enable TeamViewer
#   services.teamviewer.enable = true;
#
#   # Enable fwupd
#   services.fwupd.enable = true;
#
#   # set the video drivers to modesetting so no other drivers are loaded
#   services.xserver.videoDrivers = lib.mkForce ["modesetting"];
#
#   # Give people part of the video group access to adjust the backlight
#   services.udev.extraRules = ''
#     ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
#     ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
#   '';
# }
