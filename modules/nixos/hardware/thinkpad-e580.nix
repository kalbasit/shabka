{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "thinkpad-e580") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
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

    boot.loader.systemd-boot.enable = true;

    nix.maxJobs = lib.mkDefault 8;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    boot.kernelPackages = pkgs.linuxPackages_latest;

    console.font = "Lat2-Terminus16";
  };
}
