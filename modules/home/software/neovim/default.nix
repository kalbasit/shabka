{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.neovim;

  my_plugins = pkgs.callPackage ./plugins.lib.nix {};

in {
  options.mine.neovim = {
    enable = mkEnableOption "neovim";

    extraRC = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra NeoVim init configuration.
      '';
    };

    extraKnownPlugins = mkOption {
      default = {};
      description = ''
        Extra NeoVim known plugins.
      '';
    };

    extraPluginDictionaries = mkOption {
      type = with types; listOf attrs;
      default = [];
      description = ''
        Extra NeoVim plugin dictionary.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.neovim = {
      enable = true;

      viAlias = true;
      vimAlias = true;

      withPython = true;
      extraPythonPackages = ps: with ps; [neovim];

      withPython3 = true;
      extraPython3Packages = ps: with ps; [neovim];

      configure = {
        customRC = cfg.extraRC + (builtins.readFile (pkgs.substituteAll {
          src = ./init.vim;

          ag_bin = "${pkgs.ag}/bin/ag";
          gocode_bin = "${pkgs.nur.repos.kalbasit.gocode}/bin/gocode";
          neovim_node_host_bin = "${pkgs.nodePackages.neovim}/bin/neovim-node-host";
          typescript_server_bin = "${pkgs.nodePackages.typescript}/bin/tsserver";
          xsel_bin = "${pkgs.xsel}/bin/xsel";
        })) + (if config.mine.useColemakKeyboardLayout then (builtins.readFile ./colemak.vim) else "");

        vam.knownPlugins = pkgs.vimPlugins // my_plugins // cfg.extraKnownPlugins;
        vam.pluginDictionaries = cfg.extraPluginDictionaries ++ [
          {
            names =
              [ # vimPlugins
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
                "goyo"
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
                "direnv-vim"
                "traces-vim"
                "vim-PreserveNoEOL"
                "vim-better-whitespace"
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
              ]
              ++ (if config.mine.useColemakKeyboardLayout then ["vim-colemak"] else []);
          }
        ];
      };
    };
  };
}
