# vim:foldmethod=marker:foldlevel=0:

{ pkgs }:

let
  my_plugins = import ./plugins.nix { inherit (pkgs) vimUtils fetchFromGitHub; };

in with pkgs; neovim.override {
  vimAlias = true;

  withPython = true;
  extraPythonPackages = [python27Packages.neovim];

  withPython3 = true;
  extraPython3Packages = [python36Packages.neovim];

  configure = {
    customRC = builtins.readFile ./init.vim;

    vam.knownPlugins = vimPlugins // my_plugins;
    vam.pluginDictionaries = [
      { names = [
        "Gist"
        "Gundo"
        "LanguageClient-neovim"
        "ack-vim"
        "ale"
        "auto-pairs"
        "caw"
        "deoplete-go"
        "deoplete-nvim"
        "easy-align"
        "easymotion"
        "editorconfig-vim"
        "fugitive"
        "fzf-vim"
        "fzfWrapper"
        "multiple-cursors"
        "nvim-completion-manager"
        "polyglot"
        "repeat"
        "rhubarb"
        "sleuth"
        "surround"
        "vim-PreserveNoEOL"
        "vim-addon-nix"
        "vim-airline"
        "vim-airline-themes"
        "vim-better-whitespace"
        "vim-colemak"
        "vim-color-seoul256"
        "vim-csv"
        "vim-emmet"
        "vim-eunuch"
        "vim-go"
        "vim-markdown"
        "vim-pig"
        "vim-signify"
        "vim-speeddating"
        "vim-terraform"
        "vim-typescript"
        "vim-vissort"
        "vim-yats"
        "vim-zoomwintab"

        # "vim-maktaba"
        # "vim-bazel"
      ]; }
    ];
  };
}
