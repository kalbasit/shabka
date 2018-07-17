{pkgs, stdenv}:

stdenv.mkDerivation rec {
  name = "rbrowser";

  phases = [ "installPhase" "fixupPhase" ];

  src = ./.;

  installPhase = ''
    install -D -m755 $src/bin/rbrowser $out/bin/rbrowser
    substituteInPlace $out/bin/rbrowser \
      --subst-var-by chromium_bin ${pkgs.chromium}/bin/chromium \
      --subst-var-by firefox_bin ${pkgs.firefox}/bin/firefox \
      --subst-var-by rofi_bin ${pkgs.rofi}/bin/rofi \
      --subst-var-by zsh_bin ${pkgs.zsh}/bin/zsh \
      --subst-var-by zsh_config_dir ${pkgs.zsh-config}
  '';
}
