{stdenv, rofi}:

stdenv.mkDerivation rec {
  name = "rofi-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome/.config/rofi
    substitute $src/config $out/userHome/.config/rofi/config \
      --subst-var-by rofi_dir ${rofi}
  '';
}
