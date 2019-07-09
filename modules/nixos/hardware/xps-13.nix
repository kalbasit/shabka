{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "xps-13") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
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

    boot.loader.systemd-boot.enable = false;

    nix.maxJobs = lib.mkDefault 8;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    services.xserver.videoDrivers = lib.mkForce ["modesetting"];

    boot.kernelPackages = pkgs.linuxPackages_latest;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}

