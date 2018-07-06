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
    cp -dr --no-preserve='ownership' less $out/userHome/.less
  '';
}
