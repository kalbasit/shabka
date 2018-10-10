{ config, lib, ... }:

with lib;

{
  options.mine.workstation.virtualbox.enable = mkEnableOption "Enable VirtualBox";

  config = mkIf config.mine.workstation.virtualbox.enable {
    # Enable Virtualbox support
    virtualisation.virtualbox.host.enable = true;

    # install virtualbox extensions pack
    nixpkgs.config.virtualbox.enableExtensionPack = true;
  };
}

