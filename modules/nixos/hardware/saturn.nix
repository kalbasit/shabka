{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.mine.hardware.machine == "saturn") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
    boot.kernelModules = [ "kvm-intel" "iscsi_tcp" ];
    boot.extraModulePackages = [ ];

    boot.loader.grub = {
      configurationLimit = 30;
      device = "/dev/sda";
      enable = true;
      enableCryptodisk = true;
      version = 2;
    };

    nix.maxJobs = lib.mkDefault 32;

    mine.serial_console.enable = true;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}
