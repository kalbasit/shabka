# TODO(high): Surfingkeys must be composed of two files, the main one and the colemak bindings.
{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.chromium.enable = mkEnableOption "workstation.chromium";

  config = mkIf config.mine.workstation.chromium.enable {
    home.file.".config/chromium/profiles/epita/.keep".text = "";
    home.file.".config/chromium/profiles/lamacorp/.keep".text = "";
    home.file.".config/chromium/profiles/personal/.keep".text = "";

    home.packages = with pkgs; [
      chromium
    ];

    home.file.".surfingkeys.js".text = builtins.readFile (pkgs.substituteAll {
      src = ./surfingkeys.js;

      home_dir = "${config.home.homeDirectory}";
    });
  };
}
