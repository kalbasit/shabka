{ stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "sway-config";

  phases = [ "installPhase" "fixupPhase" ];

  src = ./.;

  installPhase = ''
    install -d -m755 $out/userHome/.config/sway

    cp -dr $src/bin $out/bin
    substituteInPlace $out/bin/sway-run \
      --subst-var-by nvim_dir ${pkgs.neovim} \
      --subst-var-by out_dir $out

    cp -dr $src/config.d $out/userHome/.config/sway/config.d

    substitute $src/config $out/userHome/.config/sway/config \
      --subst-var-by brightnessctl_bin ${pkgs.brightnessctl}/bin/brightnessctl \
      --subst-var-by i3lock_bin ${pkgs.i3lock}/bin/i3lock \
      --subst-var-by i3status_bin ${pkgs.i3status}/bin/i3status \
      --subst-var-by notify-send_bin ${pkgs.libnotify}/bin/notify-send \
      --subst-var-by pactl_bin ${pkgs.pulseaudio}/bin/pactl \
      --subst-var-by rbrowser_bin ${pkgs.rbrowser}/bin/rbrowser \
      --subst-var-by rofi_bin ${pkgs.rofi}/bin/rofi \
      --subst-var-by slack_bin ${pkgs.slack}/bin/slack \
      --subst-var-by alacritty_bin ${pkgs.alacritty}/bin/alacritty

    substitute $src/zshenv $out/userHome/.zshenv \
      --subst-var-by out_dir $out
  '';
}
