{stdenv, less}:

stdenv.mkDerivation rec {
  name = "less-config";

  phases = [ "buildPhase" "installPhase" ];

  src = ./.;

  buildInputs = [ less ];

  buildPhase = ''
    lesskey -o less $src/lesskey
  '';

  installPhase = ''
    install -dm 755 $out/userHome
    cp less $out/userHome/.less
  '';
}
