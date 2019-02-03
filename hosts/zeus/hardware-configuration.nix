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
      device = "/dev/disk/by-uuid/ba45c08d-1708-457b-9c76-0ef5ceaf3cee";
      keyFile = "/dev/disk/by-uuid/c9ae674b-6ec3-4ac9-bc13-3a409cdf352a";
    };

    cryptroot = {
      device = "/dev/disk/by-uuid/b6f30e7a-d1f2-43bb-825f-77c0c8f0f435";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/509f93b9-65cb-4886-b6eb-797697373a7d";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptstorage = {
      device = "/dev/disk/by-uuid/1bb05e34-5aa0-419b-a6c0-2574b7566832";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  fileSystems = nfsFSEntries // {
    "/" = {
      device = "/dev/disk/by-uuid/471c4bf2-14c9-4eef-a791-8beebfcfe31a";
      fsType = "btrfs";
      options = [ "subvol=@nixos/@root" ];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/471c4bf2-14c9-4eef-a791-8beebfcfe31a";
      fsType = "btrfs";
      options = [ "subvol=@nixos/@home" ];
    };

    "/yl/code" = {
      device = "/dev/disk/by-uuid/471c4bf2-14c9-4eef-a791-8beebfcfe31a";
      fsType = "btrfs";
      options = [ "subvol=@code" ];
    };

    "/yl/private" = {
      device = "/dev/disk/by-uuid/471c4bf2-14c9-4eef-a791-8beebfcfe31a";
      fsType = "btrfs";
      options = [ "subvol=@private" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/0EC6-C400";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/f58da878-7e18-430e-ad8c-321f63c61a4e"; }
  ];
}
