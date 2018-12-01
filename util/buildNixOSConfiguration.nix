{ ... }:

let

  shim = {
    boot.loader.systemd-boot.enable = true;

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/00000000-0000-0000-0000-000000000000";
      fsType = "btrfs";
    };

    nixpkgs.config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

in {

  buildNixOSConfiguration = {conf, withShim ? false}: (import <nixpkgs/nixos> {
    # configuration = conf;
    configuration.imports = [conf] ++ (if withShim then [ shim ] else []);
  }).system;
}
