{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.chromium.enable = mkEnableOption "workstation.chromium";

  config = mkIf config.mine.workstation.chromium.enable {
    home.file.".config/chromium/profiles/anya/.keep".text = "";
    home.file.".config/chromium/profiles/ihab/.keep".text = "";
    home.file.".config/chromium/profiles/nosecurity/.keep".text = "";
    home.file.".config/chromium/profiles/personal/.keep".text = "";
    home.file.".config/chromium/profiles/publica/.keep".text = "";
    home.file.".config/chromium/profiles/vanya/.keep".text = "";

    home.file.".config/chromium/profiles/nosecurity/.cmdline_args".text = ''
      --disable-web-security
    '';

    home.packages = with pkgs; [
      chromium
    ];

    home.file.".surfingkeys.js".text = builtins.readFile (pkgs.substituteAll {
      src = ./surfingkeys.js;

      home_dir = "/home/kalbasit"; # TODO: set this from the config
    });
  };
}
