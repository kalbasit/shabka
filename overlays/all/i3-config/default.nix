{ stdenv, pkgs, myHostname ? "hades" }:

stdenv.mkDerivation rec {
  name = "i3-config";

  phases = [
    "installPhase"
    "fixupPhase"
  ];

  src = ./.;

  installPhase = ''
    install -d -m755 $out/userHome/.config/i3

    cp -dr $src/bin $out/bin

    install -Dm644 $src/zsh/functions/cow $out/zsh/functions/cow
    install -d -m755 $out/userHome/.zsh/rc.d
    substitute $src/zsh/i3-fpath.zsh $out/userHome/.zsh/rc.d/i3-fpath.zsh \
      --subst-var-by out_dir $out
}
