{ vimUtils, fetchFromGitHub, stdenv }:

with vimUtils;

let

  airlineSeoul256Theme = stdenv.mkDerivation rec {
    name = "airline-seoul256-theme-${version}";
    version = "0.0.1";
    src = ./airline-seoul256.vim;
    phases = [ "installPhase" ];
    installPhase = ''
      install -Dm644 $src $out/autoload/airline/themes/seoul256.vim
    '';
  };

in {
  airline-seoul256-theme = buildVimPluginFrom2Nix {
    name = "airline-seoul256-theme-2018-08-12";
    src = airlineSeoul256Theme;
    dependencies = [];
  };

  vim-color-seoul256 = buildVimPluginFrom2Nix {
    name = "vim-color-seoul256-2017-09-04";
    src = fetchFromGitHub {
      owner = "junegunn";
      repo = "seoul256.vim";
      rev = "1475b7610663c68aa90b6e565997c8792ce0d222";
      sha256 = "03gqw14f5cirivcg1p06g500ns066yv5rd0z3zikvn4ql7n278dk";
    };
    dependencies = [];
  };
}
