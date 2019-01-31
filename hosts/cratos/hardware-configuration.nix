{ lib, ... }:

with lib;

let
  rootDevice   = "/dev/disk/by-uuid/9ee04791-04d5-4796-86d6-71fa8ca4932c";
  bootDevice   = "/dev/disk/by-uuid/4733-872F";
  swapDevice   = "/dev/disk/by-uuid/62947932-6c57-422f-9221-2a9b017ab158";

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
    cryptkey     = { device = "/dev/disk/by-uuid/5d474d32-db9f-4df4-bd3b-92ffd938c6e4"; };
    cryptroot    = { device = "/dev/disk/by-uuid/39d30d0a-8918-4f67-9359-ed9165f321e2"; keyFile = "/dev/mapper/cryptkey"; };
    cryptswap    = { device = "/dev/disk/by-uuid/20735bf9-a766-444b-8604-1004cd3e2bf4"; keyFile = "/dev/mapper/cryptkey"; };
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
