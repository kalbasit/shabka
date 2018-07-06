{stdenv}:

stdenv.mkDerivation rec {
  name = "most-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    cp -dr --no-preserve='ownership' $src/mostrc $out/userHome/.mostrc
  '';
}
