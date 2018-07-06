{stdenv}:

stdenv.mkDerivation rec {
  name = "zsh-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    cp -dr --no-preserve='ownership' $src/bin $out/bin
    cp -dr --no-preserve='ownership' $src/libexec $out/libexec
    cp -dr --no-preserve='ownership' $src/zsh $out/zsh

    substituteInPlace $out/zsh/functions.zsh \
      --subst-var-by out_dir $out

    substitute $src/zshrc $out/userHome/.zshrc \
      --subst-var-by out_dir $out
  '';
}
