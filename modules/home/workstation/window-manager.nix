{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.windowManager = mkOption rec {
    type = types.enum ["i3" "plasma5" "gnome3"];
    default = "i3";
    description = ''
      The window manager to use. Currently supported window managers are ${concatStringsSep ", " type}.
    '';
  };

  config.mine.workstation.i3.enable = if config.mine.windowManager == "i3" then true else false;
}
