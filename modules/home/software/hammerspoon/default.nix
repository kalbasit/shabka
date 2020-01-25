{ config, lib, ... }:

with lib;

let
  cfg = config.shabka.hammerspoon;
  shabka = import <shabka> { };
in {
  options.shabka.hammerspoon.enable = mkEnableOption "Enable hammerspoon (only relevant for Darwin hosts).";

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.shabka.darwinConfig != {};
        message = "Hammerspoon isn't used on Linux hosts. It should thus be disabled on them.";
      }
    ];
    home.file.".hammerspoon/init.lua".text = ''
      hs.loadSpoon('ControlEscape'):start()
    '';
    home.file.".hammerspoon/Spoons/ControlEscape.spoon/init.lua".source = "${shabka.external.hammerspoon.controlescape.path}/init.lua";
  };
}
