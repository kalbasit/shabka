{ config, lib, ... }:

with lib;

{
  config = mkIf (config.mine.hardware.machine == "precision-7530") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    boot.loader.systemd-boot.editor = false;
    boot.loader.systemd-boot.enable = true;

    boot.loader.efi.canTouchEfiVariables = true;

    nix.maxJobs = lib.mkDefault 12;
    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    services.xserver.videoDrivers = lib.mkForce ["modesetting"];

    boot.kernelPackages = pkgs.linuxPackages_latest;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}
