{ lib, ... }:

with lib;

let
  rootDevice   = "/dev/disk/by-uuid/e2b2367b-f458-4c55-a3f0-87cef3366d62";
  bootDevice   = "/dev/disk/by-uuid/CA50-980F";
  swapDevice   = "/dev/disk/by-uuid/a3e2591f-a1d4-4d75-b09c-094416b485c4";
  storgeDevice = "/dev/disk/by-uuid/d8a3aad7-3fe8-4986-acc5-c6f7525c9af4";

  subVolumes =
    {
      # NixOS
      "/"                      = { device = rootDevice;   subvol = "@nixos/@root"; };
      "/home"                  = { device = rootDevice;   subvol = "@nixos/@home"; };
      "/yl"                    = { device = rootDevice;   subvol = "@yl"; };
      "/yl/code"               = { device = rootDevice;   subvol = "@code"; options = [ "X-mount.mkdir=0700" ]; };
      "/yl/private"            = { device = rootDevice;   subvol = "@private"; options = [ "X-mount.mkdir=0700" ]; };
      "/yl/storage"            = { device = storgeDevice; subvol = "@home-kalbasit-storage"; };
    };

  mkBtrfsSubvolume = mountPoint: { device, subvol, options ? [] }:
    nameValuePair
      (mountPoint)
      ({
        inherit device;
        fsType = "btrfs";
        options =
          [ "subvol=${subvol}" ]
          ++ options;
      });

in {
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.luks.devices = {
    cryptkey     = { device = "/dev/disk/by-uuid/3830842b-a589-45e0-a51d-4ed516472575"; };
    cryptroot    = { device = "/dev/disk/by-uuid/ec133595-6294-4756-95ea-3891297f6477"; keyFile = "/dev/mapper/cryptkey"; };
    cryptswap    = { device = "/dev/disk/by-uuid/71aab354-3872-4316-a26c-b4090d73275c"; keyFile = "/dev/mapper/cryptkey"; };
    cryptstorage = { device = "/dev/disk/by-uuid/d339f141-7544-44d4-89ec-312af0e087cc"; keyFile = "/dev/mapper/cryptkey"; };
  };

  fileSystems = mergeAttrs
    (mapAttrs' mkBtrfsSubvolume subVolumes)
    {
      # NixOS

      "/boot" = { device = bootDevice; fsType = "vfat"; };
      "/mnt/volumes/root" = { device = rootDevice; fsType = "btrfs"; };

      # Storage

      "/mnt/volumes/storage" = { device = storgeDevice; fsType = "btrfs"; };
    };

  swapDevices = [ { device = swapDevice; } ];
}
