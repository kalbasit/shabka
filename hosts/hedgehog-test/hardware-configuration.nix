{ lib, ... }:

{
  imports = [ ];

  boot.initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/dda51a88-ebbe-40001-b218-e5634f2ef195";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/ac862d02-e6c2-4d98-9da5-acb819f09270"; } ];

  nix.maxJobs = lib.mkDefault 1;
  virtualisation.virtualbox.guest.enable = true;
}