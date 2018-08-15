{ config, pkgs, lib, ... }:

{
  # Include the results of the hardware scan.
  imports = [
    <nixos-hardware/common/cpu/intel>
    <nixos-hardware/common/pc/laptop>
    <nixos-hardware/common/pc/laptop/ssd>

    ./hardware-configuration.nix

    ../../cfg/common.nix
    ../../cfg/virtualisation.nix
    ../../cfg/redshift.nix

    ../../cfg/snapper.nix
  ];

  # boot the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Define your hostname.
  networking.hostName = "zeus";

  # select a console font
  i18n.consoleFont = "Lat2-Terminus16";
  boot.earlyVconsoleSetup = true;

  # put /tmp on tmpfs
  boot.tmpOnTmpfs = true;

  # List services that you want to enable:

  # enable nix-serve
  services.nix-serve = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/zeus.nasreddine.com.key) {
    enable = true;
    secretKeyFile = "/private/network-secrets/nix/caches/zeus.nasreddine.com.key";
  };
  networking.firewall.allowedTCPPorts = lib.mkIf (builtins.pathExists /private/network-secrets/nix/caches/zeus.nasreddine.com.key) [ 5000 ];

  # Enable fwupd
  services.fwupd.enable = true;

  # set the video drivers to modesetting so no other drivers are loaded
  services.xserver.videoDrivers = lib.mkForce ["modesetting"];
}
