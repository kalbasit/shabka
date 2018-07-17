{ stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "dunst-config";

  src = ./.;

  phases = [ "installPhase" ];

  installPhase = ''
    install -Dm644 $src/dunstrc $out/userHome/.config/dunst/dunstrc
    substituteInPlace $out/userHome/.config/dunst/dunstrc \
      --subst-var-by rbrowser_bin ${pkgs.rbrowser}/bin/rbrowser \
      --subst-var-by rofi_bin ${pkgs.rofi}/bin/rofi
  '';
}
