{ config, lib, ... }:

with lib;

let cfg = config.mine.virtualisation.virtualbox;
in {
  options.mine.virtualisation.virtualbox.enable = mkEnableOption "Enable VirtualBox";

  config = mkIf cfg.enable {
    # Enable Virtualbox support
    virtualisation.virtualbox.host.enable = true;

    # install virtualbox extensions pack
    nixpkgs.config.virtualbox.enableExtensionPack = true;
  };
}
