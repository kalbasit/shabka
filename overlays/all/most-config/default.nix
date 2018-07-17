{stdenv}:

stdenv.mkDerivation rec {
  name = "most-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    cp $src/mostrc $out/userHome/.mostrc
  '';
}
