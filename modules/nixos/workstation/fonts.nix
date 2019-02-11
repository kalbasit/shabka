{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.fonts.enable = mkEnableOption "workstation.fonts";

  config = mkIf config.mine.workstation.fonts.enable {
    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
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

        unstable.b612
      ];
    };
  };
}

# References:
# - https://github.com/grahamc/nixos-config/blob/7b34cbea59b78a3b61e7a955b874ca414f182bd9/main-configuration.nix#L167-L182
