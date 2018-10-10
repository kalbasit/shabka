{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.i3.enable = mkEnableOption "workstation.i3";

  config = mkIf config.mine.workstation.i3.enable {
    home.file.".config/i3status/config".text = builtins.readFile ./i3status-config;
    xsession.windowManager.i3 = import ./i3-config.lib.nix { inherit pkgs; };
  };
}
