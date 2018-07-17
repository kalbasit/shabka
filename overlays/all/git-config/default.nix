{stdenv, pkgs}:

stdenv.mkDerivation rec {
  name = "git-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm 755 $out/userHome
    substitute $src/gitconfig $out/userHome/.gitconfig \
      --subst-var-by nvim_bin ${pkgs.neovim}/bin/nvim
    cp $src/gitignore_global $out/userHome/.gitignore_global
  '';
}
