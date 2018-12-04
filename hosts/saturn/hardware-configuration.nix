{ config, lib, pkgs, ... }:

let
  exports = [
    "Anime"
    "Cartoon"
    "Code"
    "Documentaries"
    "Downloads"
    "Imported"
    "Mail"
    "Movies"
    "MusicVideos"
    "Plays"
    "Plex"
    "Stand-upComedy"
    "TVShows"
    "docker"
    "homes"
    "music"
  ];

  toFSEntry = export: lib.nameValuePair "/nas/${export}" {
    device = "172.25.3.2:/volume1/${export}";
    fsType = "nfs";
  };

  nfsFSEntries = builtins.listToAttrs (map toFSEntry exports);

in {
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-uuid/b9431758-3e7d-46dd-aaa0-36d71cba4128";
    };

    cryptroot = {
      device = "/dev/disk/by-uuid/c54db2b3-cc5f-4d75-9c33-3a0d431c7933";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/75caa509-6a0e-4b14-a60d-0942a3992bd2";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  # TODO(high): uncomment when full VLAN network is available
  fileSystems = /* nfsFSEntries // */ {
    "/" = {
      device = "/dev/disk/by-uuid/62cd917a-7553-4b0a-9c2d-5336ec375c29";
      fsType = "btrfs";
      options = [ "subvol=@nixos" ];
    };

    "/yl" = {
      device = "/dev/disk/by-uuid/62cd917a-7553-4b0a-9c2d-5336ec375c29";
      fsType = "btrfs";
      options = [ "subvol=@yl" ];
    };

    "/yl/code" = {
      device = "/dev/disk/by-uuid/62cd917a-7553-4b0a-9c2d-5336ec375c29";
      fsType = "btrfs";
      options = [ "subvol=@code" ];
    };

    "/yl/private" = {
      device = "/dev/disk/by-uuid/62cd917a-7553-4b0a-9c2d-5336ec375c29";
      fsType = "btrfs";
      options = [ "subvol=@private" ];
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/d616115d-b6fe-40cd-a868-d765d36f5592"; }
  ];
}
