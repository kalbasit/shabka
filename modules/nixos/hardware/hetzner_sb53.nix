{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "hetzner_sb53") {
    boot.initrd.availableKernelModules = [ "ahci" "igb" "usbhid" "sd_mod" ];
    boot.initrd.kernelModules = [ "dm-snapshot" ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    nix.maxJobs = mkDefault 12;

    boot.kernelPackages = pkgs.linuxPackages_latest;

    console.font = "Lat2-Terminus16";
  };
}
