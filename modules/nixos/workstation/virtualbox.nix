{ config, lib, ... }:

with lib;

{
  options.mine.workstation.virtualbox.enable = mkEnableOption "Enable VirtualBox";

  config = mkIf config.mine.workstation.virtualbox.enable {
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;

    mine.userGroups = ["vboxusers"];
  };
}
