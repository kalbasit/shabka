{stdenv, tmux, xsel, gist, libnotify}:

stdenv.mkDerivation rec {
  name = "tmux-config";

  phases = [ "installPhase" ];

  src = ./.;

  # TODO: the tmux-linux and tmux-osx must be controlled by nix and not by tmux
  # itself.
  installPhase = ''
    install -d 755 $out/userHome
    cp $src/tmux.conf $out/userHome/.tmux.conf
    substitute $src/tmux-linux.conf $out/userHome/.tmux-linux.conf \
      --subst-var-by tmux_bin ${tmux}/bin/tmux \
      --subst-var-by xsel_bin ${xsel}/bin/xsel \
      --subst-var-by gist_bin ${gist}/bin/gist \
      --subst-var-by notify-send_bin ${libnotify}/bin/notify-send
    cp $src/tmux-osx.conf $out/userHome/.tmux-osx.conf
  '';
}
