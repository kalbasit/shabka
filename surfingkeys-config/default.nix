{stdenv}:

stdenv.mkDerivation rec {
  name = "surfingkeys-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    substitute $src/surfingkeys.js $out/userHome/.surfingkeys.js \
      --subst-var-by home_dir /home/kalbasit
  '';
}
