{stdenv}:

stdenv.mkDerivation rec {
  name = "termite-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome/.config/termite
    cp $src/config $out/userHome/.config/termite/config
  '';
}
