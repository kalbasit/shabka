{ config, pkgs, lib, ... }:

with lib;

let

  font-b612 = with pkgs; stdenv.mkDerivation {
    name = "b612-font-2019-01-21";

    src = fetchgit {
      url = "git://git.polarsys.org/gitroot/b612/b612.git";
      rev = "bd14fde2544566e620eab106eb8d6f2b7fb1347e";
      sha256 = "1w1w9za599w3asmdkhng9amb9w0riq6mg400p43w1qnj0zqazy3d";
    };

    buildPhase = ":";

    installPhase = ''
      mkdir -p $out/share/fonts
      cp ./TTF/*.ttf $out/share/fonts
    '';

    meta = {
      homepage = "http://b612-font.com/";
      platforms = stdenv.lib.platforms.all;
      license = stdenv.lib.licenses.epl10;
      maintainers = [ stdenv.lib.maintainers.grahamc ];
    };
  };

in {
  options.mine.workstation.fonts.enable = mkEnableOption "workstation.fonts";

  config = mkIf config.mine.workstation.fonts.enable {
    fonts = {
      enableDefaultFonts = true;
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

        # TODO: This has already landed in unstable!
        font-b612
      ];
    };
  };
}

# References:
# - https://github.com/grahamc/nixos-config/blob/7b34cbea59b78a3b61e7a955b874ca414f182bd9/main-configuration.nix#L167-L182
