{ stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "zsh-config";

  phases = [ "installPhase" "fixupPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    cp -dr $src/bin $out/bin
    cp -dr $src/libexec $out/libexec
    cp -dr $src/zsh $out/zsh

    substituteInPlace $out/zsh/functions.zsh \
      --subst-var-by out_dir $out

    substitute $src/zshrc $out/userHome/.zshrc \
      --subst-var-by out_dir $out \
      --subst-var-by direnv_dir ${pkgs.direnv}

    substituteInPlace $out/zsh/functions/tmycli \
      --subst-var-by mycli_bin ${pkgs.mycli}/bin/mycli \
      --subst-var-by netstat_bin ${pkgs.nettools}/bin/netstat \
      --subst-var-by ssh_bin ${pkgs.openssh}/bin/ssh
  '';
}
