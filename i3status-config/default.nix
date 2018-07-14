{ pkgs, stdenv }:

stdenv.mkDerivation rec {
  name = "i3status-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -Dm644 $src/config $out/userHome/.config/i3status/config
  '';
}
