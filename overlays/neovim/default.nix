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
      customRC = builtins.readFile (super.substituteAll {
        src = ./init.vim;

        gocode_bin = "${self.gocode}/bin/gocode";
        neovim_node_host_bin = "${self.nodePackages.neovim}/bin/neovim-node-host";
        typescript_server_bin = "${self.nodePackages.typescript}/bin/tsserver";
        xsel_bin = "${self.xsel}/bin/xsel";
      });

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
            # "vim-typescript"    # TODO: https://github.com/kalbasit/dotfiles/issues/15
            "vim-yats"

            # required by Gist
            # TODO: https://github.com/NixOS/nixpkgs/pull/43399
            "webapi-vim"

            "vim-maktaba"
            "vim-bazel"
          ];
        }
      ];
    };
  };

  # TODO: remove the following override once the below PR is merged.
  # - https://github.com/NixOS/nixpkgs/pull/43995
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs(oa: {
    postInstall = ''
      sed -i -e "s|'xsel|'${super.xsel}/bin/xsel|g" $out/share/nvim/runtime/autoload/provider/clipboard.vim
    '';
  });
}
