{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "hetzner_cloud") {
    boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sd_mod" "sr_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

    boot.loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
      enableCryptodisk = true;
      configurationLimit = 30;
    };

    nix.maxJobs = lib.mkDefault 1;

    boot.kernelPackages = pkgs.linuxPackages_latest;

    i18n.consoleFont = "Lat2-Terminus16";
  };
}
