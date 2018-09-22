{ pkgs, ... }:

{
  home.file.".config/i3status/config".text = builtins.readFile ./i3status-config;
  xsession.windowManager.i3 = import ./i3-config.nix { inherit pkgs; };
}
