{ pkgs, stdenv }:

stdenv.mkDerivation rec {
  name = "i3-config";

  phases = [ "installPhase" "fixupPhase" ];

  src = ./.;

  installPhase = ''
    install -d -m755 $out/userHome/.config/i3

    cp -dr $src/bin $out/bin

    substitute $src/config $out/userHome/.config/i3/config \
      --subst-var-by brightnessctl_bin ${pkgs.brightnessctl}/bin/brightnessctl \
      --subst-var-by i3lock_bin ${pkgs.i3lock}/bin/i3lock \
      --subst-var-by i3status_bin ${pkgs.i3status}/bin/i3status \
      --subst-var-by notify-send_bin ${pkgs.libnotify}/bin/notify-send \
      --subst-var-by pactl_bin ${pkgs.pulseaudio}/bin/pactl \
      --subst-var-by rbrowser_bin ${pkgs.rbrowser}/bin/rbrowser \
      --subst-var-by rofi_bin ${pkgs.rofi}/bin/rofi \
      --subst-var-by slack_bin ${pkgs.slack}/bin/slack \
      --subst-var-by termite_bin ${pkgs.termite}/bin/termite
  '';
}
