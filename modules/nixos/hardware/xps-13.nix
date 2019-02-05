{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.mine.hardware.machine == "xps-13") {
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

    # use unstable kernel and Virtualbox from unstable so it compiles
    boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
    virtualisation.virtualbox.host.package = pkgs.unstable.virtualbox;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}

