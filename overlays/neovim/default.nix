self: super:

let
  my_plugins = import ./plugins.nix { inherit (super) vimUtils fetchFromGitHub; };
in

{
  neovim = super.neovim.override {
    vimAlias = true;

    withPython = true;
    extraPythonPackages = [self.python27Packages.neovim];

    withPython3 = true;
    extraPython3Packages = [self.python36Packages.neovim];

    configure = {
      customRC = builtins.readFile ./init.vim;

      vam.knownPlugins = super.vimPlugins // my_plugins;
      vam.pluginDictionaries = [
        {
          names = [
            "Gist"
            "Gundo"
            "LanguageClient-neovim"
            "ack-vim"
            "ale"
            "auto-pairs"
            "caw"
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
            "vim-vissort"
            "vim-zoomwintab"

            ## DeoPlete completion support
            "deoplete-nvim"

            # Golang support
            "deoplete-go"

            # Typescript support
            "vim-typescript"
            "vim-yats"

            # required by Gist
            # TODO: https://github.com/NixOS/nixpkgs/pull/43399
            "webapi-vim"

            # "vim-maktaba"
            # "vim-bazel"
          ];
        }
      ];
    };
  };
}
