{ lib, ... }:

with lib;

let
  rootDevice   = "/dev/disk/by-uuid/1da7cc56-a28d-4333-ab70-7a595f7df47c";
  bootDevice   = "/dev/disk/by-uuid/ADE1-3154";
  swapDevice   = "/dev/disk/by-uuid/a54fc702-d678-4bb1-9448-940f2337cefe";

  subVolumes =
    {
      # NixOS
      "/"                      = { device = rootDevice;   subvol = "@nixos/@root"; };
      "/home"                  = { device = rootDevice;   subvol = "@nixos/@home"; };
      "/yl"                    = { device = rootDevice;   subvol = "@yl"; };
      "/yl/code"               = { device = rootDevice;   subvol = "@code"; options = [ "X-mount.mkdir=0700" ]; };
      "/yl/private"            = { device = rootDevice;   subvol = "@private"; options = [ "X-mount.mkdir=0700" ]; };
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
    cryptkey     = { device = "/dev/disk/by-uuid/9e7505bf-4be5-47aa-8225-bed182cf58f2"; };
    cryptroot    = { device = "/dev/disk/by-uuid/f9dff108-7369-4dbe-9fe5-46e795fd7b25"; keyFile = "/dev/mapper/cryptkey"; };
    cryptswap    = { device = "/dev/disk/by-uuid/7cd0fad9-4af1-40af-9576-af9c6212d9b9"; keyFile = "/dev/mapper/cryptkey"; };
  };

  fileSystems = mergeAttrs
    (mapAttrs' mkBtrfsSubvolume subVolumes)
    {
      # NixOS

      "/boot" = { device = bootDevice; fsType = "vfat"; };
      "/mnt/volumes/root" = { device = rootDevice; fsType = "btrfs"; };
    };

  swapDevices = [ { device = swapDevice; } ];
}
