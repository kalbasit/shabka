{ config, lib, ... }:

with lib;

let cfg = config.mine.virtualisation.libvirtd;

in {
  options.mine.virtualisation.libvirtd.enable = mkEnableOption "Enable Docker";

  config = mkIf cfg.enable {
    virtualisation.libvirtd.enable = true;

    mine.userGroups = ["libvirtd"];
  };
}
