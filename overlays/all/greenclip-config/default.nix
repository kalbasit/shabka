{ stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "greenclip-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -Dm644 $src/greenclip.cfg $out/userHome/.config/greenclip.cfg
  '';
}
