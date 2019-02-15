{ lib, ... }:

with lib;

let
  rootDevice   = "/dev/disk/by-uuid/80a02fae-e860-4e9d-945b-63fe9ece252f";
  bootDevice   = "/dev/disk/by-uuid/2E37-9536";
  swapDevice   = "/dev/disk/by-uuid/a7e5c6a5-79e1-4256-aa3a-e0a92ef526e0";

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
    cryptkey     = { device = "/dev/disk/by-uuid/7cb0ed7a-eb28-4a92-8280-5bc3e68cb4a3"; };
    cryptroot    = { device = "/dev/disk/by-uuid/a9e0d610-e61c-4852-a4bb-eacdf7291dd4"; keyFile = "/dev/mapper/cryptkey"; };
    cryptswap    = { device = "/dev/disk/by-uuid/7be87c50-1beb-435e-bdf6-a16925cd890e"; keyFile = "/dev/mapper/cryptkey"; };
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
