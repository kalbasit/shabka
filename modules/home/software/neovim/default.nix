{ config, pkgs, lib, ... }:

with lib;

let
  my_plugins = pkgs.callPackage ./plugins.lib.nix {};
in {
  options.mine.neovim.enable = mkEnableOption "neovim";

  config = mkIf config.mine.neovim.enable {
    programs.neovim = {
      enable = true;

      viAlias = true;
      vimAlias = true;

      withPython = true;
      extraPythonPackages = ps: with ps; [neovim];

      withPython3 = true;
      extraPython3Packages = ps: with ps; [neovim];

      configure = {
        customRC = builtins.readFile (pkgs.substituteAll {
          src = ./init.vim;

          ag_bin = "${pkgs.ag}/bin/ag";
          gocode_bin = "${pkgs.nur.repos.kalbasit.gocode}/bin/gocode";
          neovim_node_host_bin = "${pkgs.nodePackages.neovim}/bin/neovim-node-host";
          typescript_server_bin = "${pkgs.nodePackages.typescript}/bin/tsserver";
          xsel_bin = "${pkgs.xsel}/bin/xsel";
        });

        vam.knownPlugins = pkgs.vimPlugins // my_plugins;
        vam.pluginDictionaries = [
          {
            names = [ # vimPlugins
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
              "fzf-vim"
              "fzfWrapper"
              "multiple-cursors"
              "nvim-completion-manager"
              "polyglot"
              "repeat"
              "rhubarb"
              "sleuth"
              "surround"
              "vim-airline"
              "vim-airline-themes"
              "vim-eunuch"
              "vim-go"
              "vim-markdown"
              "vim-signify"
              "vim-speeddating"
              "vimtex"

              ## DeoPlete completion support
              "deoplete-nvim"

              # Golang support
              "deoplete-go"

              # required by Gist
              # TODO: https://github.com/NixOS/nixpkgs/pull/43399
              "webapi-vim"

              "vim-maktaba"
              "vim-bazel"
            ] ++ [ # my_plugins
              "airline-seoul256-theme"
              "traces-vim"
              "vim-PreserveNoEOL"
              "vim-better-whitespace"
              "vim-colemak"
              "vim-color-seoul256"
              "vim-csv"
              "vim-emmet"
              "vim-fugitive" # update to 2.4
              "vim-pig"
              "vim-terraform"
              "vim-vissort"
              "vim-zoomwintab"

              # Typescript support
              # "vim-typescript"    # TODO: https://github.com/kalbasit/dotfiles/issues/15
              "vim-yats"
            ];
          }
        ];
      };
    };
  };
}
