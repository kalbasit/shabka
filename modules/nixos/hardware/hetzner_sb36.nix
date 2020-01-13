{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "hetzner_sb36") {
    boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" "r8169" ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    nix.maxJobs = mkDefault 8;

    boot.kernelPackages = pkgs.linuxPackages_latest;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}
