# TODO(low): Refactor how this is wired up. Basically I had to hack
# config.shabka.workstation.i3.enable assignment line below to test for
# nixosConfig != {} before setting itself as it was getting set on Darwin!

{ config, pkgs, lib, ... }:

with lib;

{
  options.shabka.windowManager = mkOption rec {
    type = types.enum ["i3" "plasma5" "gnome3"];
    default = "i3";
    description = ''
      The window manager to use. Currently supported window managers are ${concatStringsSep ", " type}.
    '';
  };

  config.shabka.workstation.i3.enable = if config.shabka.nixosConfig != {} && config.shabka.windowManager == "i3" then true else false;
}
