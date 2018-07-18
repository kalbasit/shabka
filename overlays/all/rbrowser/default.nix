{ pkgs, stdenv, makeDesktopItem }:

let
  desktopItem = makeDesktopItem {
    categories = "GTK;Network;WebBrowser;";
    desktopName = "Relay Browser";
    exec = "rbrowser %U";
    genericName  = "Web Browser";
    icon = "chromium";
    mimeType = "x-scheme-handler/unknown;x-scheme-handler/about;text/html;text/xml;application/xhtml+xml;text/mml;x-scheme-handler/http;x-scheme-handler/https;";
    name = "rbrowser";
  };

in stdenv.mkDerivation rec {
  name = "rbrowser";

  phases = [ "installPhase" "fixupPhase" ];

  src = ./.;

  installPhase = ''
    install -D -m755 $src/bin/rbrowser $out/bin/rbrowser
    substituteInPlace $out/bin/rbrowser \
      --subst-var-by chromium_bin ${pkgs.chromium}/bin/chromium \
      --subst-var-by firefox_bin ${pkgs.firefox}/bin/firefox \
      --subst-var-by rofi_bin ${pkgs.rofi}/bin/rofi \
      --subst-var-by zsh_bin ${pkgs.zsh}/bin/zsh \
      --subst-var-by zsh_config_dir ${pkgs.zsh-config}

    install -dm755 $out/userHome/.local/share/applications
    ln -s ${desktopItem}/share/applications/* $out/userHome/.local/share/applications/
  '';
}
