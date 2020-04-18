{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "precision-7530") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    boot.loader.grub = {
      configurationLimit = 30;
      device = "nodev";
      efiSupport = true;
      enable = true;
      enableCryptodisk = true;
    };

    boot.loader.efi.canTouchEfiVariables = true;

    boot.loader.systemd-boot.enable = mkDefault false;

    nix.maxJobs = mkDefault 12;

    powerManagement.cpuFreqGovernor = mkDefault "powersave";

    services.xserver.videoDrivers = mkForce ["modesetting"];

    boot.kernelPackages = pkgs.linuxPackages_latest;

    console.font = "Lat2-Terminus16";
  };
}
