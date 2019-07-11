{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "zeus") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
    boot.kernelModules = [ "kvm-intel" "iscsi_tcp" ];
    boot.extraModulePackages = [ ];

    boot.loader.grub = {
      configurationLimit = 30;
      device = "nodev";
      efiSupport = true;
      enable = true;
      enableCryptodisk = true;
    };

    boot.loader.efi.canTouchEfiVariables = true;

    nix.maxJobs = lib.mkDefault 12;

    boot.kernelPackages = pkgs.linuxPackages_latest;

    shabka.serial_console.enable = true;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}
