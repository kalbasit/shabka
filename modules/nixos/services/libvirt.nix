{ config, lib, ... }:

with lib;

let cfg = config.shabka.virtualisation.libvirtd;

in {
  options.shabka.virtualisation.libvirtd.enable = mkEnableOption "Enable libvirtd";

  config = mkIf cfg.enable {
    virtualisation.libvirtd.enable = true;
    virtualisation.libvirtd.qemuRunAsRoot = false;

    shabka.users.groups = ["libvirtd"];
  };
}
