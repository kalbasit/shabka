{stdenv, brightnessctl, pulseaudio, i3lock, rofi, termite, libnotify, slack, zsh-config, nvim-config}:

stdenv.mkDerivation rec {
  name = "sway-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -d -m755 $out/userHome/.config/sway

    cp -dr $src/bin $out/bin
    substituteInPlace $out/bin/relay-browser \
      --subst-var-by zsh_dir ${zsh-config}
    substituteInPlace $out/bin/sway-run \
      --subst-var-by nvim_dir ${nvim-config}

    cp -dr $src/config.d $out/userHome/.config/sway/config.d

    substitute $src/config $out/userHome/.config/sway/config \
      --subst-var-by out_dir $out \
      --subst-var-by brightnessctl_bin ${brightnessctl}/bin/brightnessctl \
      --subst-var-by i3lock_bin ${i3lock}/bin/i3lock \
      --subst-var-by notify-send_bin ${libnotify}/bin/notify-send \
      --subst-var-by pactl_bin ${pulseaudio}/bin/pactl \
      --subst-var-by rofi_bin ${rofi}/bin/rofi \
      --subst-var-by slack_bin ${slack}/bin/slack \
      --subst-var-by termite_bin ${termite}/bin/termite
  '';
}
