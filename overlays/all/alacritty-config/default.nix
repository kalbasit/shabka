{ stdenv }:

stdenv.mkDerivation rec {
  name = "alacritty-config";

  src = ./.;

  phases = [ "installPhase" ];

  installPhase = ''
    install -Dm644 $src/alacritty.yml $out/userHome/.config/alacritty/alacritty.yml
  '';
}
