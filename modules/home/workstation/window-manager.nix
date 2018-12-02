# TODO(low): Refactor how this is wired up. Basically I had to hack
# config.mine.workstation.i3.enable assignment line below to test for
# nixosConfig != {} before setting itself as it was getting set on Darwin!

{ config, pkgs, lib, ... }:

with lib;

let

  windowManagerConfig = if config.mine.windowManager == "i3" then {
    home.file.".config/i3status/config".text = builtins.readFile ./i3/i3status-config;
    xsession = {
      windowManager  = {
        i3 = import ./i3/i3-config.lib.nix { inherit config pkgs; };
      };
    };
  } else {};

in {
  options.mine.windowManager = mkOption rec {
    type = types.enum ["i3" "plasma5" "gnome3"];
    default = "i3";
    description = ''
      The window manager to use. Currently supported window managers are ${concatStringsSep ", " type}.
    '';
  };

  config.mine.workstation.i3.enable = if config.mine.nixosConfig != {} && config.mine.windowManager == "i3" then true else false;
}
