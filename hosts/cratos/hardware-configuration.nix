{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-uuid/4d340cea-b271-4540-88ba-d57829622e38";
    };

    cryptroot = {
      device = "/dev/disk/by-uuid/3da97a3e-98bc-4947-83a9-5c7647db3150";
      keyFile = "/dev/mapper/cryptkey";
    };

    cryptswap = {
      device = "/dev/disk/by-uuid/9c521d99-67b5-4b77-b6d2-ee152ccad85b";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/de2f32b1-85d0-49ab-8684-019175e09544";
      fsType = "btrfs";
      options = [ "subvol=@nixos/@root" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/02EA-F418";
      fsType = "vfat";
    };

    "/home" = {
      device = "/dev/disk/by-uuid/de2f32b1-85d0-49ab-8684-019175e09544";
      fsType = "btrfs";
      options = [ "subvol=@nixos/@home" ];
    };

    "/home/kalbasit/code" = {
      device = "/dev/disk/by-uuid/de2f32b1-85d0-49ab-8684-019175e09544";
      fsType = "btrfs";
      options = [ "subvol=@code" "X-mount.mkdir=0700" ];
    };

    "/home/kalbasit/private" = {
      device = "/dev/disk/by-uuid/de2f32b1-85d0-49ab-8684-019175e09544";
      fsType = "btrfs";
      options = [ "subvol=@private" "X-mount.mkdir=0700" ];
    };

    "/mnt/volumes/root" = {
      device = "/dev/disk/by-uuid/de2f32b1-85d0-49ab-8684-019175e09544";
      fsType = "btrfs";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/be829e9b-e163-45fb-beea-aa4cf1aa6012"; }
  ];
}
