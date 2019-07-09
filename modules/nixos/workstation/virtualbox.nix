{ config, lib, ... }:

with lib;

{
  options.shabka.workstation.virtualbox.enable = mkEnableOption "Enable VirtualBox";

  config = mkIf config.shabka.workstation.virtualbox.enable {
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;

    shabka.users.groups = ["vboxusers"];
  };
}
