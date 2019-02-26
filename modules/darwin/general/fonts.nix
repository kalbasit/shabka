{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.fonts.enable = mkEnableOption "workstation.fonts";

  config = mkIf config.mine.fonts.enable {
    fonts = {
      enableFontDir = true;
      fonts = with pkgs; [
        powerline-fonts
        source-code-pro
        # TODO: this package should work on Darwin
        # twemoji-color-font

        noto-fonts
        # TODO: noto-fonts-extra is failing
        # while setting up the build environment: executing '/nix/store/2dr15pm102nzxzsy3hibh1m0bjpp3rws-bash-4.4-p23/bin/bash': Argument list too long
        # noto-fonts-extra
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
