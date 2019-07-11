{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  options.shabka.workstation.fonts.enable = mkEnableOption "workstation.fonts";

  config = mkIf config.shabka.workstation.fonts.enable {
    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        twemoji-color-font
        hack-font

        powerline-fonts
        source-code-pro
        twemoji-color-font

        noto-fonts
        noto-fonts-extra
        noto-fonts-emoji
        noto-fonts-cjk

        symbola

        # helvetica
        vegur # the official NixOS font

        shabka.external.nixpkgs.release-unstable.b612
      ];
    };
  };
}

# References:
# - https://github.com/grahamc/nixos-config/blob/7b34cbea59b78a3b61e7a955b874ca414f182bd9/main-configuration.nix#L167-L182
